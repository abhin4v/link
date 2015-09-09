module Link.Client where

import Control.Concurrent hiding (forkFinally)
import Control.Exception  hiding (handle)
import Control.Monad      (void)
import Data.Time          (getCurrentTime, diffUTCTime)
import System.IO          (hGetLine)
import System.Timeout     (timeout)
import Text.Printf        (printf)

import qualified Data.Map.Strict as Map

import Link.Protocol
import Link.Types
import Link.Util

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action fun =
  mask $ \restore ->
    forkIO (do r <- try (restore action); fun r)

race :: IO a -> IO b -> IO (Either a b)
race ioa iob = do
  m <- newEmptyMVar
  bracket (forkFinally (fmap Left ioa) (putMVar m)) killThread $ \_ ->
    bracket (forkFinally (fmap Right iob) (putMVar m)) killThread $ \_ -> do
      r <- readMVar m
      case r of
        Left e  -> throwIO e
        Right a -> return a

sendMessage :: Client -> Message -> IO ()
sendMessage Client {..} = writeChan clientChan

sendResponse :: Client -> Message -> IO ()
sendResponse Client {..} = printToHandle clientHandle . formatMessage

runClient :: Server -> Client -> IO ()
runClient Server {..} client@Client {..} = do
  clientAlive <- newMVar True
  pingThread <- forkIO $ ping clientAlive
  run clientAlive `finally` killThread pingThread
  where
    pingDelay       = 120
    pingDelayMicros = pingDelay * 1000 * 1000

    ping clientAlive = do
      sendMessage client Ping
      threadDelay pingDelayMicros
      now <- getCurrentTime
      pongTime <- readMVar clientPongTime
      if diffUTCTime now pongTime > fromIntegral pingDelay
        then void $ swapMVar clientAlive False
        else ping clientAlive

    run clientAlive = do
      alive <- readMVar clientAlive
      if not alive
        then printf "Client timed out: %s\n" (userName clientUser)
        else do
          r <-  try . timeout pingDelayMicros $ race readCommand readMessage
          case r of
            Left (e :: SomeException) -> printf "Exception: %s\n" (show e)
            Right g -> case g of
              Nothing -> run clientAlive
              Just cm -> do
                case cm of
                  Left mcommand -> case mcommand of
                    Nothing      -> printf "Could not parse command\n"
                    Just command -> handleCommand command
                  Right message -> sendResponse client message
                run clientAlive

    readCommand = do
      command <- hGetLine clientHandle
      printf "<%s>: %s\n" (userName clientUser) command
      return $ parseCommand command

    readMessage = readChan clientChan

    handleCommand (PrivMsg user msg) =
      withMVar serverUsers $ \clientMap ->
        case Map.lookup user clientMap of
          Nothing      -> sendResponse client $ NoSuchUser (userName user)
          Just client' -> sendMessage client' $ PrivMsg clientUser msg
    handleCommand Pong = do
      now <- getCurrentTime
      void $ swapMVar clientPongTime now
