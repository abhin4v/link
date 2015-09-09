module Link.Server where

import Control.Exception  (finally)
import Control.Concurrent (forkFinally, newMVar, modifyMVar, modifyMVar_, newChan,
                           writeChan, withMVar)
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

sendMessage :: Message -> Client -> IO ()
sendMessage message Client {..} = writeChan clientChan message

checkAddClient :: Server -> User -> Handle -> IO (Maybe Client)
checkAddClient Server {..} user@User {..} handle = do
  modifyMVar serverUsers $ \clientMap ->
    if Map.member user clientMap
      then return (clientMap, Nothing)
      else do
        clientChan <- newChan
        let client = Client user handle clientChan
        printf "New user connected: %s\n" userName
        return (Map.insert user client clientMap, Just client)

runClient :: Server -> Client -> IO ()
runClient Server {..} Client {..} = forever $ do
  command <- hGetLine clientHandle
  printf "<%s>: %s\n" (userName clientUser) command
  case parseCommand command of
    Nothing -> return ()
    Just com -> handleCommand com
  where
    handleCommand message@(PrivMsg user _) =
      withMVar serverUsers $ \clientMap ->
        case Map.lookup user clientMap of
          Nothing -> printf "No such user: %s\n" (userName user)
          Just client -> sendMessage message client

removeClient :: Server -> User -> IO ()
removeClient Server {..} user =
  modifyMVar_ serverUsers $ \clientMap ->
    return $ Map.delete user clientMap
