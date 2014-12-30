{-# LANGUAGE RecordWildCards, LambdaCase, MultiWayIf, TupleSections, BangPatterns #-}

module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import XLibBindings
import Control.Applicative
import Foreign.C.String
import System.Exit
import System.Environment
import Data.List
import Data.Bits

herbstIPCArgsAtom :: String
herbstIPCArgsAtom = "_HERBST_IPC_ARGS"

herbstIPCOutputAtom :: String
herbstIPCOutputAtom = "_HERBST_IPC_OUTPUT"

herbstIPCStatusAtom :: String
herbstIPCStatusAtom = "_HERBST_IPC_EXIT_STATUS"

herbstIPCClass :: String
herbstIPCClass = "HERBST_IPC_CLASS"

herbstHookWinIdAtom :: String
herbstHookWinIdAtom = "__HERBST_HOOK_WIN_ID"

data HCConnection = HCConnection {
  display :: Display,
  atomArgs :: Atom,
  atomOutput :: Atom,
  atomStatus :: Atom,
  root :: Window
}

hcConnect :: IO HCConnection
hcConnect = do
  d <- openDefaultDisplay

  hcConnectToDisplay d

hcConnectToDisplay :: Display -> IO HCConnection
hcConnectToDisplay display = do
  let root = defaultRootWindow display
  atomArgs <- internAtom display herbstIPCArgsAtom False
  atomOutput <- internAtom display herbstIPCOutputAtom False
  atomStatus <- internAtom display herbstIPCStatusAtom False

  return $ HCConnection {..}

hcDisconnect :: HCConnection -> IO ()
hcDisconnect con = closeDisplay (display con)

hcCreateClientWindow :: HCConnection -> IO Window
hcCreateClientWindow con = do
  grabServer (display con)

  win <- createSimpleWindow (display con) (root con) 42 42 42 42 0 0 0

  setClassHint (display con) win $
    (ClassHint herbstIPCClass herbstIPCClass)

  selectInput (display con) win propertyChangeMask

  ungrabServer (display con)

  return win

hcDestroyClientWindow :: HCConnection -> Window -> IO ()
hcDestroyClientWindow con = destroyWindow (display con)

hcSendCommand :: HCConnection -> Window -> [String] -> IO (Maybe (String, Int))
hcSendCommand con win args = do
  textProp <- utf8TextListToTextProperty (display con) args
  setTextProperty' (display con) win textProp (atomArgs con)
  allocaXEvent $ readCommandOutput

  where readCommandOutput :: XEventPtr -> IO (Maybe (String, Int))
        readCommandOutput event = do
          first <- readOutputEvent event
          case first of
           Left (Just output) ->
             fmap (output,) <$> execUntil getRight (readOutputEvent event)
           Right (Just status) ->
             fmap (,status) <$> execUntil getLeft (readOutputEvent event)
           _ -> return Nothing


        execUntil :: (Monad m) => (a -> Maybe b) -> m a -> m b
        execUntil p m = m >>= \a -> case p a of
          Just b -> return b
          Nothing -> execUntil p m

        getLeft :: Either a b -> Maybe a
        getLeft (Left x) = Just x
        getLeft (Right _) = Nothing

        getRight :: Either a b -> Maybe b
        getRight (Left _) = Nothing
        getRight (Right x) = Just x

        readOutputEvent :: XEventPtr -> IO (Either (Maybe String) (Maybe Int))
        readOutputEvent event = do
          nextEvent (display con) event
          getEvent event >>= \case
            PropertyEvent{..} | ev_window == win ->
              if | ev_atom == (atomOutput con) -> Left <$> readOutput
                 | ev_atom == (atomStatus con) -> Right <$> readStatus
                 | otherwise -> readOutputEvent event
            _ -> readOutputEvent event

        readOutput :: IO (Maybe String)
        readOutput = do
          tp <- getTextProperty (display con) win (atomOutput con)
          utf8str <- internAtom (display con) "UTF8_STRING" False
          if tp_encoding tp == sTRING || tp_encoding tp == utf8str
            then Just <$> peekCString (tp_value tp)
            else return Nothing

        readStatus :: IO (Maybe Int)
        readStatus = fmap (fromIntegral . head) <$>
          getWindowProperty32 (display con) (atomStatus con) win

hcHookConnect :: HCConnection -> IO (Maybe Window)
hcHookConnect con = do
  let root = defaultRootWindow (display con)
  atom <- internAtom (display con) herbstHookWinIdAtom False
  getWindowProperty32 (display con) atom root >>= \case
    Just (winid:_) -> do
      let win = fromIntegral winid
          mask = structureNotifyMask .|. propertyChangeMask

      selectInput (display con) win mask

      return $ Just win
    _ -> return Nothing

hcNextHook :: HCConnection -> Window -> IO [String]
hcNextHook con win = allocaXEvent $ \event -> loop event
  where loop event = do
          nextEvent (display con) event
          getEvent event >>= \case
            PropertyNotification{..}
              | ev_window == win && ev_propstate /= propertyDelete -> do
                  prop <- getTextProperty (display con) win ev_atom

withHC :: (HCConnection -> Window -> IO a) -> IO a
withHC f = do
  con <- hcConnect
  win <- hcCreateClientWindow con
  res <- f con win
  hcDestroyClientWindow con win
  hcDisconnect con
  return res

putStrMaybeLn :: String -> IO ()
putStrMaybeLn str
  | "\n" `isSuffixOf` str = putStr str
  | otherwise = putStrLn str

main :: IO ()
main = do
  args <- getArgs
  withHC (\con win -> hcSendCommand con win args) >>= \case
    Just (res,0) -> putStrMaybeLn res
    Just (res,status) -> putStrLn res >> exitWith (ExitFailure status)
    _ -> putStrLn $ "An error occured while reading response"
