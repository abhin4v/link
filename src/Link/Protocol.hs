module Link.Protocol where

import Text.Printf (printf)

import Link.Types

parseCommand :: String -> Maybe Message
parseCommand command = case words command of
  ["PONG"]               -> Just Pong
  "MSG" : userName : msg -> Just $ Msg (User userName) (unwords msg)
  ["QUIT"]               -> Just $ Quit
  _                      -> Nothing

formatMessage :: Message -> String
formatMessage (MsgReply user msg) = printf "MSG %s %s" (userName user) msg
formatMessage (NameInUse name)    = printf "NAMEINUSE %s" name
formatMessage (Connected name)    = printf "CONNECTED %s" name
formatMessage Ping                = "PING"
formatMessage (NoSuchUser name)   = printf "NOSUCHUSER %s" name
