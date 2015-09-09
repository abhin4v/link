module Link.Protocol where

import Text.Printf (printf)

import Link.Types

parseCommand :: String -> Maybe Message
parseCommand command = case words command of
  ["PONG"]                   -> Just Pong
  "PRIVMSG" : userName : msg -> Just $ PrivMsg (User userName) (unwords msg)
  _                          -> Nothing

formatMessage :: Message -> String
formatMessage (PrivMsg user msg) = printf "PRIVMSG %s %s" (userName user) msg
formatMessage (NameInUse name)   = printf "NAMEINUSE %s" name
formatMessage (Connected name)   = printf "CONNECTED %s" name
formatMessage Ping               = "PING"
formatMessage (NoSuchUser name)  = printf "NOSUCHUSER %s" name
