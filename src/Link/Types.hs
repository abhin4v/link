module Link.Types where

import System.IO (Handle)
import Control.Concurrent (MVar, Chan)
import qualified Data.Map as Map

type UserName = String

data User = User { userName :: !UserName }
            deriving (Show, Eq, Ord)

data Client = Client {
                clientUser   :: !User
              , clientHandle :: !Handle
              , clientChan   :: !(Chan Message)
              }

data Server = Server {
                serverUsers :: MVar (Map.Map User Client)
              }

data Message = NameInUse UserName
             | Connected UserName
             | PrivMsg User String
               deriving (Show, Eq)
