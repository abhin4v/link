module Link.Types where

import Control.Concurrent
import Control.Concurrent.STM (STM, TVar, TChan, newTVar, newTVarIO, newTChanIO, newBroadcastTChan)
import Data.Time              (UTCTime, getCurrentTime)
import System.IO              (Handle)

import qualified Data.Map as Map
import qualified Data.Set as Set

type UserName = String
type ChannelName = String

data User = User { userName :: !UserName }
            deriving (Show, Eq, Ord)

data Client = Client {
                clientUser         :: !User
              , clientHandle       :: !Handle
              , clientChan         :: TChan Message
              , clientPongTime     :: MVar UTCTime
              , clientChannelChans :: TVar (Map.Map ChannelName (TChan Message))
              }

newClient :: User -> Handle -> IO Client
newClient user handle = do
  clientChan         <- newTChanIO
  now                <- getCurrentTime
  clientPongTime     <- newMVar now
  clientChannelChans <- newTVarIO Map.empty
  return $ Client user handle clientChan clientPongTime clientChannelChans

data Channel = Channel {
              channelName  :: !ChannelName
            , channelUsers :: TVar (Set.Set User)
            , channelChan  :: TChan Message
            }

newChannel :: ChannelName -> Set.Set User -> STM Channel
newChannel channelName users = do
  channelUsers <- newTVar users
  channelChan  <- newBroadcastTChan
  return $ Channel channelName channelUsers channelChan

data Server = Server {
                serverUsers    :: MVar (Map.Map User Client)
              , serverChannels :: TVar (Map.Map ChannelName Channel)
              }

newServer :: IO Server
newServer = do
  serverUsers    <- newMVar Map.empty
  serverChannels <- newTVarIO Map.empty
  return $ Server serverUsers serverChannels

data Message = NameInUse UserName
             | LoggedIn UserName
             | Ping
             | MsgReply User String
             | TellReply ChannelName User String
             | NoSuchUser UserName
             | Joined ChannelName User
             | Leaved ChannelName User
             | NamesReply ChannelName (Set.Set User)
             | Pong
             | Login UserName
             | Msg User String
             | Tell ChannelName String
             | Join ChannelName
             | Leave ChannelName
             | Names ChannelName
             | Quit
               deriving (Show, Eq)
