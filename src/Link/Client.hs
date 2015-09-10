module Link.Client where

import Control.Concurrent.STM
import Control.Concurrent     hiding (forkFinally)
import Control.Exception      hiding (handle)
import Control.Monad          (void, forever, when, unless, forM_)
import Data.Time              (getCurrentTime, diffUTCTime)
import System.IO              (hGetLine, Handle)
import System.Timeout         (timeout)
import Text.Printf            (printf, hPrintf)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Link.Protocol
import Link.Types

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action fun =
  mask $ \restore ->
    forkIO (do r <- try (restore action); fun r)

sendMessage :: Client -> Message -> STM ()
sendMessage Client {..} = writeTChan clientChan

sendMessageIO :: Client -> Message -> IO ()
sendMessageIO client = atomically . sendMessage client

tellMessage :: Channel -> Message -> STM ()
tellMessage Channel {..} = writeTChan channelChan

printToHandle :: Handle -> String -> IO ()
printToHandle handle = hPrintf handle "%s\n"

sendResponse :: Client -> Message -> IO ()
sendResponse Client {..} = printToHandle clientHandle . formatMessage

runClient :: Server -> Client -> IO ()
runClient Server {..} client@Client {..} = do
  clientAlive   <- newMVar True
  pingThread    <- forkIO $ ping clientAlive `finally` killClient clientAlive
  commandThread <- forkIO $ readCommands `finally` killClient clientAlive
  run clientAlive `finally` do
    killThread pingThread
    killThread commandThread
    clientChannelMap <- readTVarIO clientChannelChans
    forM_ (Map.keys clientChannelMap) $ \channelName ->
      handleMessage (Leave channelName) clientAlive
  where
    pingDelay       = 120
    pingDelayMicros = pingDelay * 1000 * 1000

    killClient clientAlive = void $ swapMVar clientAlive False

    ping clientAlive = do
      sendMessageIO client Ping
      threadDelay pingDelayMicros
      now      <- getCurrentTime
      pongTime <- readMVar clientPongTime
      if diffUTCTime now pongTime > fromIntegral pingDelay
        then killClient clientAlive
        else ping clientAlive

    run clientAlive = do
      alive <- readMVar clientAlive
      if not alive
        then printf "Closing connection: %s\n" (userName clientUser)
        else do
          r <-  try . timeout pingDelayMicros . atomically $ do
            clientChannelMap <- readTVar clientChannelChans
            foldr (orElse . readTChan) retry $ clientChan : Map.elems clientChannelMap
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

    handleMessage (Join channelName) _ = atomically $ do
      clientChannelMap <- readTVar clientChannelChans
      unless (Map.member channelName clientChannelMap) $ do
        channelMap  <- readTVar serverChannels
        channel@Channel {channelChan} <- case Map.lookup channelName channelMap of
          Just (channel@Channel {channelUsers}) -> do
            modifyTVar' channelUsers $ Set.insert clientUser
            return channel
          Nothing                               -> do
            channel <- newChannel channelName $ Set.singleton clientUser
            modifyTVar' serverChannels $ Map.insert channelName channel
            return channel
        clientChannelChan <- dupTChan channelChan
        modifyTVar' clientChannelChans $ Map.insert channelName clientChannelChan
        tellMessage channel $ Joined channelName clientUser

    handleMessage (Leave channelName) _ = atomically $ do
      channelMap <- readTVar serverChannels
      case Map.lookup channelName channelMap of
        Just (channel@Channel {channelUsers}) -> do
          modifyTVar' channelUsers $ Set.delete clientUser
          users <- readTVar channelUsers
          when (Set.null users) $
            modifyTVar' serverChannels $ Map.delete channelName
          modifyTVar' clientChannelChans $ Map.delete channelName
          tellMessage channel $ Leaved channelName clientUser
        Nothing                               -> return ()

    handleMessage (Names channelName) _ = atomically $ do
      channelMap <- readTVar serverChannels
      users      <- case Map.lookup channelName channelMap of
        Just (Channel {channelUsers}) -> readTVar channelUsers
        Nothing                       -> return Set.empty
      sendMessage client $ NamesReply channelName users

    handleMessage (Tell channelName msg) _ = atomically $ do
      channelMap <- readTVar serverChannels
      case Map.lookup channelName channelMap of
        Just channel -> tellMessage channel $ TellReply channelName clientUser msg
        Nothing      -> return ()

    handleMessage message _            = sendResponse client message
