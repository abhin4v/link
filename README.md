A simple mutli-threaded chat server written in Haskell with support for
user-to-user private messaging and channels which users can join and
chat in.

Run server:

```
$ link <port>
```

Command to the server over TCP:

```
$ telnet <host> <port>
```

Commands sent to the server:

- `LOGIN <username>` Login with the username
- `QUIT` Quit the server
- `MSG <username> <message>` Send a private message to a user
- `JOIN <channel>` Join a channel
- `LEAVE <channel>` Leave a channel
- `TELL <channel> <message>` Send a message to a channel
- `NAMES <channel>` Get names of all users in a channel
- `PONG` Reply to a ping from the server

Messages from the server:

- `LOGGEDIN <username>` Confirmation of a login
- `NAMEINUSE <username>` Username is already taken
- `MSG <username> <message>` A private message from a user
- `NOSUCHUSER <username>` You tried to send a message to a non-existent user
- `JOINED <channel> <username>` A user joined the channel you have joined
- `LEFT <channel> <username>` A user left the channel you have joined
- `NAMES <channel> <username> <username> ...` Names of all users in a channel
- `TELL <channel> <username> <message>` A message from a channel you have joined
- `PING` A ping from the server. Must be replied with a `PONG` command or else
   the server will disconnect you.
