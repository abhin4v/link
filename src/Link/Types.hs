module Link.Types where

import Control.Concurrent     (MVar)
import Control.Concurrent.STM (TVar, TChan)
import Data.Time              (UTCTime)
import System.IO              (Handle)

import qualified Data.Map as Map
import qualified Data.Set as Set

type UserName = String
type RoomName = String

data User = User { userName :: !UserName }
            deriving (Show, Eq, Ord)

data Client = Client {
                clientUser     :: !User
              , clientHandle   :: !Handle
              , clientChan     :: !(TChan Message)
              , clientPongTime :: MVar UTCTime
              }

data Server = Server {
                serverUsers :: MVar (Map.Map User Client)
              }

data Message = NameInUse UserName
             | Connected UserName
             | Ping
             | MsgReply User String
             | NoSuchUser UserName
             | Pong
             | Msg User String
             | Quit
               deriving (Show, Eq)
