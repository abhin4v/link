module Link.Protocol where

import Text.Printf (printf)

import qualified Data.Set as Set

import Link.Types

parseCommand :: String -> Maybe Message
parseCommand command = case words command of
  ["PONG"]                   -> Just Pong
  "LOGIN" : userName         -> Just $ Login (unwords userName)
  "MSG" : userName : msg     -> Just $ Msg (User userName) (unwords msg)
  "TELL" : channelName : msg -> Just $ Tell channelName (unwords msg)
  ["QUIT"]                   -> Just Quit
  "JOIN" : channelName       -> Just $ Join (unwords channelName)
  "LEAVE" : channelName      -> Just $ Leave (unwords channelName)
  "NAMES" : channelName      -> Just $ Names (unwords channelName)
  _                          -> Nothing

formatMessage :: Message -> String
formatMessage (MsgReply user msg)       = printf "MSG %s %s" (userName user) msg
formatMessage (NameInUse name)          = printf "NAMEINUSE %s" name
formatMessage (LoggedIn name)           = printf "LOGGEDIN %s" name
formatMessage Ping                      = "PING"
formatMessage (NoSuchUser name)         = printf "NOSUCHUSER %s" name
formatMessage (Joined channelName user) = printf "JOINED %s %s" channelName (userName user)
formatMessage (Leaved channelName user) = printf "LEFT %s %s" channelName (userName user)
formatMessage (TellReply channelName user msg) =
  printf "TELL %s %s %s" channelName (userName user) msg
formatMessage (NamesReply channelName users)   =
  printf "NAMES %s %s" channelName . unwords . map userName . Set.toList $ users
formatMessage msg = error $ printf "Cannot format message: %s" (show msg)
