module Link.Server where

import Control.Concurrent
import Control.Exception  hiding (handle)
import Control.Monad      (forever, join, void)
import Network.Socket     ( AddrInfo (..), AddrInfoFlag(..), SocketType(..)
                          , SocketOption(..), withSocketsDo, accept
                          , socketToHandle, defaultHints, getAddrInfo, socket
                          , setSocketOption, bind, listen)
import System.IO          (hClose, hSetNewlineMode, hSetBuffering, BufferMode(..),
                           IOMode(..), universalNewlineMode, hGetLine, Handle, stdout)
import System.Timeout     (timeout)
import Text.Printf        (printf)

import qualified Data.Map.Strict as Map

import Link.Client
import Link.Protocol
import Link.Types

runServer :: String -> Int -> IO ()
runServer host port = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  server <- newServer
  sock <- newSocket
  printf "Listening on port %d\n" port
  forever $ do
     (sock', addr) <- accept sock
     printf "Accepted connection from %s\n" (show addr)
     handle <- socketToHandle sock' ReadWriteMode
     void $ forkFinally (connectClient server handle) (\_ -> hClose handle)
  where
    newSocket = do
      let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
                               , addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))
      sock   <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

connectClient :: Server -> Handle -> IO ()
connectClient server handle = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
  where
    waitDelay       = 60
    waitDelayMicros = waitDelay * 1000 * 1000

    readName = do
      command <- timeout waitDelayMicros . fmap parseCommand $ hGetLine handle
      case command of
        Nothing -> printf "Client login timed out\n" >> return ()
        Just (Just (Login name)) -> do
          let user = User name
          ok <- checkAddClient server user handle
          case ok of
            Nothing     -> do
              printToHandle handle . formatMessage $ NameInUse name
              readName
            Just client -> do
              printToHandle handle . formatMessage $ LoggedIn name
              runClient server client `finally` removeClient server user
        _ -> readName

checkAddClient :: Server -> User -> Handle -> IO (Maybe Client)
checkAddClient Server {..} user@User {..} handle =
  modifyMVar serverUsers $ \clientMap ->
    if Map.member user clientMap
      then return (clientMap, Nothing)
      else do
        client <- newClient user handle
        printf "New user connected: %s\n" userName
        return (Map.insert user client clientMap, Just client)

removeClient :: Server -> User -> IO ()
removeClient Server {..} user =
  modifyMVar_ serverUsers $ return . Map.delete user
