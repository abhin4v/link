module Link.Client where

import Control.Concurrent.STM (STM, writeTChan, readTChan, atomically, orElse)
import Control.Concurrent     hiding (forkFinally)
import Control.Exception      hiding (handle)
import Control.Monad          (void, forever)
import Data.Time              (getCurrentTime, diffUTCTime)
import System.IO              (hGetLine)
import System.Timeout         (timeout)
import Text.Printf            (printf)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Link.Protocol
import Link.Types
import Link.Util

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action fun =
  mask $ \restore ->
    forkIO (do r <- try (restore action); fun r)

sendMessage :: Client -> Message -> STM ()
sendMessage Client {..} = writeTChan clientChan

sendMessageIO :: Client -> Message -> IO ()
sendMessageIO client = atomically . sendMessage client

sendResponse :: Client -> Message -> IO ()
sendResponse Client {..} = printToHandle clientHandle . formatMessage

runClient :: Server -> Client -> IO ()
runClient Server {..} client@Client {..} = do
  clientAlive   <- newMVar True
  pingThread    <- forkIO $ ping clientAlive `finally` killClient clientAlive
  commandThread <- forkIO $ readCommands `finally` killClient clientAlive
  run clientAlive `finally` (killThread pingThread >> killThread commandThread)
  where
    pingDelay       = 120
    pingDelayMicros = pingDelay * 1000 * 1000

    killClient clientAlive = void $ swapMVar clientAlive False

    ping clientAlive = do
      sendMessageIO client Ping
      threadDelay pingDelayMicros
      now <- getCurrentTime
      pongTime <- readMVar clientPongTime
      if diffUTCTime now pongTime > fromIntegral pingDelay
        then killClient clientAlive
        else ping clientAlive

    run clientAlive = do
      alive <- readMVar clientAlive
      if not alive
        then printf "Closing connection: %s\n" (userName clientUser)
        else do
          r <-  try . timeout pingDelayMicros . atomically . readTChan $ clientChan
          case r of
            Left (e :: SomeException) -> printf "Exception: %s\n" (show e)
            Right g -> do
              case g of
                Nothing      -> return ()
                Just message -> handleMessage message clientAlive
              run clientAlive

    readCommands = forever $ do
      command <- hGetLine clientHandle
      printf "<%s>: %s\n" (userName clientUser) command
      case parseCommand command of
        Nothing -> printf "Could not parse command: %s\n" command
        Just c  -> sendMessageIO client c

    handleMessage (Msg user msg) _     =
      withMVar serverUsers $ \clientMap ->
        case Map.lookup user clientMap of
          Nothing      -> sendResponse client $ NoSuchUser (userName user)
          Just client' -> sendMessageIO client' $ MsgReply clientUser msg
    handleMessage Pong _               = do
      now <- getCurrentTime
      void $ swapMVar clientPongTime now
    handleMessage Quit clientAlive     = killClient clientAlive
    handleMessage (Join channelName) _ = do
      modifyMVar_ serverChannels $ \channelMap -> do
        case Map.lookup channelName channelMap of
          Just (Channel {channelUsers}) -> do
            modifyMVar_ channelUsers $ return . Set.insert clientUser
            return channelMap
          Nothing             -> do
            channel <- newChannel channelName $ Set.singleton clientUser
            return $ Map.insert channelName channel channelMap
    handleMessage (Leave channelName) _ = do
      modifyMVar_ serverChannels $ \channelMap -> do
        case Map.lookup channelName channelMap of
          Just (Channel {channelUsers}) -> do
            modifyMVar_ channelUsers $ return . Set.delete clientUser
            users <- readMVar channelUsers
            return $ if Set.null users
              then Map.delete channelName channelMap
              else channelMap
          Nothing             -> return channelMap
    handleMessage message _            = sendResponse client message
