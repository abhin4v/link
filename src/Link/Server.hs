module Link.Server where

import Control.Exception  (finally)
import Control.Concurrent (forkFinally, newMVar, modifyMVar, modifyMVar_)
import Control.Monad      (forever)
import Network            (withSocketsDo, listenOn, accept, PortID(..))
import System.IO          (hClose, hSetNewlineMode, hSetBuffering, BufferMode(..),
                           universalNewlineMode, hGetLine, Handle)
import Text.Printf        (printf, hPrintf)

import qualified Data.Map as Map

import Link.Types

runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
  serverUsers <- newMVar Map.empty
  let server = Server serverUsers
  sock <- listenOn . PortNumber . fromIntegral $ port
  printf "Listening on port %d\n" port
  forever $ do
     (handle, host, port') <- accept sock
     printf "Accepted connection from %s: %s\n" host (show port')
     forkFinally (talk server handle) (\_ -> hClose handle)

talk :: Server -> Handle -> IO ()
talk server handle = do
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
             runClient server client
                 `finally` removeClient server user

checkAddClient :: Server -> User -> Handle -> IO (Maybe Client)
checkAddClient Server {..} user@User {..} handle = do
  modifyMVar serverUsers $ \clientMap ->
    if Map.member user clientMap
      then return (clientMap, Nothing)
      else do
        let client = Client user handle
        printf "New user connected: %s" userName
        return (Map.insert user client clientMap, Just client)

runClient :: Server -> Client -> IO ()
runClient server Client {..} = forever $ do
  command <- hGetLine clientHandle
  print command

removeClient :: Server -> User -> IO ()
removeClient Server {..} user =
  modifyMVar_ serverUsers $ \clientMap ->
    return $ Map.delete user clientMap
