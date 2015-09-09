module Link.Protocol where

import Link.Types

parseCommand :: String -> Maybe Message
parseCommand command = case words command of
  "PRIVMSG" : userName : msg -> Just $ PrivMsg (User userName) (unwords msg)
  _  -> Nothing
