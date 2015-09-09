module Link.Server where

import Control.Exception  (finally, bracket, throwIO, try, SomeException,
                           AsyncException(ThreadKilled))
import Control.Concurrent
import Control.Monad      (forever)
import Network            (withSocketsDo, listenOn, accept, PortID(..))
import System.IO          (hClose, hSetNewlineMode, hSetBuffering, BufferMode(..),
                           universalNewlineMode, hGetLine, Handle, stdout)
import Text.Printf        (printf, hPrintf)

import qualified Data.Map.Strict as Map

import Link.Protocol
import Link.Types

runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  serverUsers <- newMVar Map.empty
  let server = Server serverUsers
  sock <- listenOn . PortNumber . fromIntegral $ port
  printf "Listening on port %d\n" port
  forever $ do
     (handle, host, port') <- accept sock
     printf "Accepted connection from %s: %s\n" host (show port')
     forkFinally (connectClient server handle) (\_ -> hClose handle)

connectClient :: Server -> Handle -> IO ()
connectClient server handle = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
  where
    readName = do
      name <- hGetLine handle
      if null name
      then readName
      else do
        let user = User name
        ok <- checkAddClient server user handle
        case ok of
          Nothing -> do
            hPrintf handle
              "The name %s is in use, please choose another\n" name
            readName
          Just client ->
            runClient server client `finally` removeClient server user

checkAddClient :: Server -> User -> Handle -> IO (Maybe Client)
checkAddClient Server {..} user@User {..} handle =
  modifyMVar serverUsers $ \clientMap ->
    if Map.member user clientMap
      then return (clientMap, Nothing)
      else do
        clientChan <- newChan
        let client = Client user handle clientChan
        printf "New user connected: %s\n" userName
        return (Map.insert user client clientMap, Just client)

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

    handleMessage (PrivMsg user msg) =
      hPrintf clientHandle "PRIVMSG %s %s\n" (userName user) msg

removeClient :: Server -> User -> IO ()
removeClient Server {..} user =
  modifyMVar_ serverUsers $ \clientMap ->
    return $ Map.delete user clientMap
