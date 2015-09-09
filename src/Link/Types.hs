module Link.Types where

import System.IO (Handle)
import Control.Concurrent (MVar, Chan)
import qualified Data.Map as Map

data User = User { userName :: !String }
            deriving (Show, Eq, Ord)

data Client = Client {
                clientUser   :: !User
              , clientHandle :: !Handle
              , clientChan   :: !(Chan Message)
              }

data Server = Server {
                serverUsers :: MVar (Map.Map User Client)
              }

data Message = PrivMsg User String
               deriving (Show, Eq)
