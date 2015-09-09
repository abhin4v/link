module Link.Types where

import Control.Concurrent (MVar, Chan)
import Data.Time (UTCTime)
import System.IO (Handle)

import qualified Data.Map as Map

type UserName = String

data User = User { userName :: !UserName }
            deriving (Show, Eq, Ord)

data Client = Client {
                clientUser     :: !User
              , clientHandle   :: !Handle
              , clientChan     :: !(Chan Message)
              , clientPongTime :: MVar UTCTime
              }

data Server = Server {
                serverUsers :: MVar (Map.Map User Client)
              }

data Message = NameInUse UserName
             | Connected UserName
             | Ping
             | Pong
             | PrivMsg User String
             | NoSuchUser UserName
               deriving (Show, Eq)
