{-# LANGUAGE RecordWildCards, LambdaCase, MultiWayIf, TupleSections, BangPatterns, ScopedTypeVariables, Rank2Types #-}

module HLWM.Client.IPC
       ( HerbstConnection
       , connect
       , disconnect
       , withConnection
       , recvEvent
       , asyncSendCommand
       , sendCommand
       , nextHook
       ) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Herbst
import Control.Applicative
import Foreign.C.String
import Data.Bits
import Data.Maybe

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

data HerbstConnection = HerbstConnection {
  display :: Display,
  atomArgs :: Atom,
  atomOutput :: Atom,
  atomStatus :: Atom,
  root :: Window,
  hooksWin :: Window,
  clientWin :: Window
}

connect :: IO (Maybe HerbstConnection)
connect = do
  display <- openDefaultDisplay

  let root = defaultRootWindow display
  atomArgs <- internAtom display herbstIPCArgsAtom False
  atomOutput <- internAtom display herbstIPCOutputAtom False
  atomStatus <- internAtom display herbstIPCStatusAtom False

  clientWin <- createClientWindow display root
  findHookWindow display root >>= \case
    Just hooksWin -> return $ Just $ HerbstConnection {..}
    Nothing -> return Nothing -- FIXME deinitialize


disconnect :: HerbstConnection -> IO ()
disconnect con = do
  destroyClientWindow con
  closeDisplay (display con)

createClientWindow :: Display -> Window -> IO Window
createClientWindow display root = do
  grabServer display

  win <- createSimpleWindow display root 42 42 42 42 0 0 0

  setClassHint display win $
    (ClassHint herbstIPCClass herbstIPCClass)

  selectInput display win propertyChangeMask

  ungrabServer display

  return win

destroyClientWindow :: HerbstConnection -> IO ()
destroyClientWindow con = destroyWindow (display con) (clientWin con)

findHookWindow :: Display -> Window -> IO (Maybe Window)
findHookWindow display root = do
  atom <- internAtom display herbstHookWinIdAtom False
  getWindowProperty32 display atom root >>= \case
    Just (winid:_) -> do
      let win = fromIntegral winid
          mask = structureNotifyMask .|. propertyChangeMask

      selectInput display win mask

      return $ Just win
    _ -> return Nothing

asyncSendCommand :: HerbstConnection -> [String] -> IO ()
asyncSendCommand con args = do
  textProp <- utf8TextListToTextProperty (display con) args
  setTextProperty' (display con) (clientWin con) textProp (atomArgs con)

data HerbstEvent = HookEvent [String]
                 | StatusEvent Int
                 | OutputEvent String

recvEvent :: HerbstConnection -> IO HerbstEvent
recvEvent con = allocaXEvent eventLoop
  where eventLoop :: XEventPtr -> IO HerbstEvent
        eventLoop event = do
          nextEvent (display con) event
          getEvent event >>= \case
            PropertyEvent{..}
              | ev_window == (clientWin con) && ev_atom == (atomOutput con) ->
                  readOutput >>= cont event OutputEvent
              | ev_window == (clientWin con) && ev_atom == (atomStatus con) ->
                  readStatus >>= cont event StatusEvent
              | ev_window == (hooksWin con) && ev_propstate /= propertyDelete ->
                  readHook ev_atom >>= cont event HookEvent
            _ -> eventLoop event

        cont :: XEventPtr -> (a -> HerbstEvent) -> Maybe a -> IO HerbstEvent
        cont event f = maybe (eventLoop event) (return . f)

        readOutput :: IO (Maybe String)
        readOutput = do
          tp <- getTextProperty (display con) (clientWin con) (atomOutput con)
          utf8str <- internAtom (display con) "UTF8_STRING" False
          if tp_encoding tp == sTRING || tp_encoding tp == utf8str
            then Just <$> peekCString (tp_value tp)
            else return Nothing

        readStatus :: IO (Maybe Int)
        readStatus = fmap (fromIntegral . head) <$>
          getWindowProperty32 (display con) (atomStatus con) (clientWin con)

        readHook :: Atom -> IO (Maybe [String])
        readHook atom = do
          prop <- getTextProperty (display con) (hooksWin con) atom
          Just <$> utf8TextPropertyToTextList (display con) prop

recvCommandOutput :: HerbstConnection -> IO (Int, String)
recvCommandOutput con = readBoth Nothing Nothing
  where readBoth (Just s) (Just o) = return (o,s)
        readBoth a b = recvEvent con >>= \case
          OutputEvent o | isNothing a -> readBoth (Just o) b
          StatusEvent s | isNothing b -> readBoth a (Just s)
          _ -> readBoth a b

sendCommand :: HerbstConnection -> [String] -> IO (Int, String)
sendCommand con args = do
  asyncSendCommand con args
  recvCommandOutput con

nextHook :: HerbstConnection -> IO [String]
nextHook con = recvEvent con >>= \case
  HookEvent r -> return r
  _           -> nextHook con

withConnection :: (HerbstConnection -> IO a) -> IO (Maybe a)
withConnection f = connect >>= \case
  Just con -> do
    res <- f con
    disconnect con
    return $ Just res
  Nothing -> return Nothing
