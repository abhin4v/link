module Link.Server where

import Control.Concurrent
import Control.Exception  hiding (handle)
import Control.Monad      (forever)
import Network            (withSocketsDo, listenOn, accept, PortID(..))
import System.IO          (hClose, hSetNewlineMode, hSetBuffering, BufferMode(..),
                           universalNewlineMode, hGetLine, Handle, stdout)
import Text.Printf        (printf)

import qualified Data.Map.Strict as Map

import Link.Client
import Link.Protocol
import Link.Types

runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  server <- newServer
  sock <- listenOn . PortNumber . fromIntegral $ port
  printf "Listening on port %d\n" port
  forever $ do
     (handle, host, port') <- accept sock
     printf "Accepted connection from %s: %s\n" host (show port')
     forkIO $ connectClient server handle `finally` hClose handle

connectClient :: Server -> Handle -> IO ()
connectClient server handle = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
  where
    readName = do
      command <- fmap parseCommand $ hGetLine handle
      case command of
        Just (Login name) -> do
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
