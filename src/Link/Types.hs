module Link.Types where

import Control.Concurrent
import Control.Concurrent.STM (TVar, TChan, newTVarIO, newTChanIO, newBroadcastTChanIO)
import Data.Time              (UTCTime, getCurrentTime)
import System.IO              (Handle)

import qualified Data.Map as Map
import qualified Data.Set as Set

type UserName = String
type ChannelName = String

data User = User { userName :: !UserName }
            deriving (Show, Eq, Ord)

data Client = Client {
                clientUser     :: !User
              , clientHandle   :: !Handle
              , clientChan     :: TChan Message
              , clientPongTime :: MVar UTCTime
              }

newClient :: User -> Handle -> IO Client
newClient user handle = do
  clientChan     <- newTChanIO
  now            <- getCurrentTime
  clientPongTime <- newMVar now
  return $ Client user handle clientChan clientPongTime

data Channel = Channel {
              channelName  :: !ChannelName
            , channelUsers :: MVar (Set.Set User)
            , channelChan  :: TChan Message
            }

newChannel :: ChannelName -> Set.Set User -> IO Channel
newChannel channelName users = do
  channelUsers <- newMVar users
  channelChan  <- newBroadcastTChanIO
  return $ Channel channelName channelUsers channelChan

data Server = Server {
                serverUsers    :: MVar (Map.Map User Client)
              , serverChannels :: MVar (Map.Map ChannelName Channel)
              }

newServer :: IO Server
newServer = do
  serverUsers    <- newMVar Map.empty
  serverChannels <- newMVar Map.empty
  return $ Server serverUsers serverChannels

data Message = NameInUse UserName
             | Connected UserName
             | Ping
             | MsgReply User String
             | NoSuchUser UserName
             | Joined ChannelName User
             | Leaved ChannelName User
             | Pong
             | Msg User String
             | Join ChannelName
             | Leave ChannelName
             | Quit
               deriving (Show, Eq)
