module Link.Client where

import Control.Exception  hiding (handle)
import Control.Concurrent hiding (forkFinally)
import Control.Monad      (forever)
import System.IO          (hGetLine)
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
  bracket (forkFinally (fmap Left ioa) (putMVar m)) cancel $ \_ ->
    bracket (forkFinally (fmap Right iob) (putMVar m)) cancel $ \_ -> do
      r <- readMVar m
      case r of
        Left e -> throwIO e
        Right a -> return a
  where
    cancel t = throwTo t ThreadKilled

sendMessage :: Message -> Client -> IO ()
sendMessage message Client {..} = writeChan clientChan message

runClient :: Server -> Client -> IO ()
runClient Server {..} Client {..} = forever $ do
  r <- try $ race readCommand readMessage
  case r of
    Left (e :: SomeException) -> printf "Exception: %s\n" (show e)
    Right cm -> case cm of
      Left mcommand -> case mcommand of
        Nothing      -> printf "Could not parse command\n"
        Just command -> handleCommand command
      Right message -> handleMessage message
  where
    readCommand = do
      command <- hGetLine clientHandle
      printf "<%s>: %s\n" (userName clientUser) command
      return $ parseCommand command

    readMessage = readChan clientChan

    handleCommand (PrivMsg user msg) =
      withMVar serverUsers $ \clientMap ->
        case Map.lookup user clientMap of
          Nothing -> printf "No such user: %s\n" (userName user)
          Just client -> sendMessage (PrivMsg clientUser msg) client

    handleMessage = printToHandle clientHandle . formatMessage
