{-# LANGUAGE LambdaCase, RecordWildCards #-}
module HLWM.Client.Concurrent
       ( Client
       , connect
       , sendCommand
       , nextHook
       ) where

import HLWM.Client.IPC hiding (sendCommand,nextHook,connect)
import qualified HLWM.Client.IPC as IPC

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Maybe

data Client = Client {
  connection :: HerbstConnection,
  commandLock :: Lock,
  eventChan :: TChan HerbstEvent
}

connect :: IO (Maybe Client)
connect = IPC.connect >>= \case
  Nothing -> return Nothing
  Just connection -> do
    commandLock <- newEmptyTMVarIO
    eventChan <- newBroadcastTChanIO
    void $ forkIO $ eventThread connection eventChan
    return $ Just $ Client {..}

sendCommand :: Client -> [String] -> IO (Int, String)
sendCommand client args = do
  events <- atomically $ do
    lock (commandLock client)
    dupTChan (eventChan client)
  asyncSendCommand (connection client) args
  res <- readBoth events Nothing Nothing
  atomically $ unlock (commandLock client)
  return res

  where readBoth _ (Just s) (Just o) = return (o,s)
        readBoth events a b = atomically (readTChan events) >>= \case
          OutputEvent o | isNothing a -> readBoth events (Just o) b
          StatusEvent s | isNothing b -> readBoth events a (Just s)
          _ -> readBoth events a b

nextHook :: Client -> IO [String]
nextHook client = do
  chan <- atomically $ dupTChan (eventChan client)

  let loop = atomically (readTChan chan) >>= \case
        HookEvent res -> return res
        _             -> loop

  loop

eventThread :: HerbstConnection -> TChan HerbstEvent -> IO ()
eventThread con chan = forever $ do
  ev <- recvEvent con
  atomically $ writeTChan chan ev

type Lock = TMVar ()

lock :: TMVar () -> STM ()
lock l = putTMVar l ()

unlock :: TMVar () -> STM ()
unlock l = takeTMVar l >> return ()
