{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import qualified Data.Text as T
import Database.SQLite.Simple
import Network.Socket
import qualified Network.Socket as N
import Network.Socket.ByteString
import Text.Parsec
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.List (intersperse)
import qualified Data.ByteString as BS
import Text.Parsec.Text
import qualified User as U
import Control.Monad (forever)

-- I will `NOTE` things that are different from the book

-- NOTE: representing request and response types will let us separate:
-- parsing, handling requests and representing responses. I left the
-- organization of the code logically untouched (aka passing the
-- database connection and the socket client to all the needed
-- functions)
type Host = T.Text
type Username = T.Text
data FingerRequest = ListAllUsersRequest (Maybe Host)
                   | AboutUserRequest Username (Maybe Host)
                   | InsertUserRequest U.User
                   | UpdateUserRequest U.User
                   deriving (Eq, Show)

data FingerResponse = ListAllUsersResponse [U.User]
                    | AboutUserResponse (Maybe U.User)
                    | InsertUserResponse (Maybe U.User)
                    | UpdateUserResponse (Maybe U.User)
                    deriving (Eq, Show)

-- NOTE: I thought that a real parser will be nice to use in a final
-- project since there's a chapter on it

-- TODO: handle verbose with `/W`
parseRequest :: T.Text -> Either ParseError FingerRequest
parseRequest = parse (commandP <* crlf) "fingerd"
  where commandP = (prefixP "/W" *> (aboutUserRequestP <|> listAllUsersRequestP))
                   <|> (prefixP "/I" *> insertUserRequestP)
                   <|> (prefixP "/U" *> updateUserRequestP)
                   <|> aboutUserRequestP
                   <|> listAllUsersRequestP
        aboutUserRequestP = AboutUserRequest <$> usernameP <*> optionMaybe hostnameP
        listAllUsersRequestP = ListAllUsersRequest <$> optionMaybe hostnameP
        insertUserRequestP = InsertUserRequest <$> (U.User (Nothing :: Maybe Integer)
                                                    <$> lexP usernameP
                                                    <*> fieldP
                                                    <*> fieldP
                                                    <*> fieldP
                                                    <*> fieldP
                                                   )
        updateUserRequestP = UpdateUserRequest <$> (U.User (Nothing :: Maybe Integer)
                                                    <$> lexP usernameP
                                                    <*> fieldP
                                                    <*> fieldP
                                                    <*> fieldP
                                                    <*> fieldP
                                                   )
        fieldP = T.pack <$> lexP (quotedP (many1 (satisfy (/= '"')) <|> tokenP))
        hostnameP = string "@" *> (T.pack <$> tokenP)
        usernameP = T.pack <$> tokenP
        tokenP = many1 alphaNum
        quotedP = between (string "\"") (string "\"")
        prefixP s = try (lexP $ string s)
        lexP :: Parser a -> Parser a
        lexP p = p <* many (char ' ')

handleRequest :: Connection -> Socket -> FingerRequest -> IO ()
handleRequest db sc (ListAllUsersRequest _) = do
  users <- query_ db U.allUsersQ
  renderResponse sc (ListAllUsersResponse users)
handleRequest db sc (AboutUserRequest u _) =
  U.userByUsername db u >>= renderResponse sc . AboutUserResponse
handleRequest db sc (InsertUserRequest u) =
  U.insertUser db u >>= renderResponse sc . InsertUserResponse
handleRequest db sc (UpdateUserRequest u) =
  U.updateUser db u >>= renderResponse sc . UpdateUserResponse

renderResponse :: Socket -> FingerResponse -> IO ()
renderResponse sc (ListAllUsersResponse users) =
  sendAll sc (encodeUtf8 response)
  where response = T.concat $ intersperse "\n" $ U.username <$> users
-- TODO: more information in case of error
renderResponse sc (AboutUserResponse u) = sendAll sc $ renderUser u "user not found"
renderResponse sc (InsertUserResponse u) = sendAll sc $ renderUser u "user not inserted"
renderResponse sc (UpdateUserResponse u) = sendAll sc $ renderUser u "user not updated"

renderUser :: Maybe U.User -> BS.ByteString -> BS.ByteString
renderUser Nothing e = BS.concat [ "ERROR: ", e, "\n" ]
-- TODO: format better
renderUser (Just u) _ = BS.concat [ "Login: ", e $ U.username u, "\t\t"
                                  , "Name: ", e $ U.realName u, "\n"
                                  , "Directory: ", e $ U.homeDirectory u, "\t\t"
                                  , "Shell: ", e $ U.shell u, "\n"]
  where e = encodeUtf8

-- TODO: something better
handleError :: Socket -> IO ()
handleError sc = sendAll sc "ERROR: unknown finger command\n"

handleRequests :: Connection -> Socket -> IO ()
handleRequests db ss = forever $ do
  (sc, _) <- accept ss
  msg <- recv sc 1024
  print $ ">> " <> (T.unpack . decodeUtf8) msg
  case parseRequest (decodeUtf8 msg) of
    (Right fingerRequest) -> handleRequest db sc fingerRequest
    (Left _) -> handleError sc
  N.close sc

-- TODO: close appropriately the socket server (ss) on SIGINT
-- TODO: specify database filename
main :: IO ()
main = withSocketsDo $ do
  addressInfo <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing
                 (Just "79")
  ss <- socket (addrFamily $ head addressInfo) Stream defaultProtocol
  N.bind ss (addrAddress $ head addressInfo)
  listen ss 1
  conn <- U.connect
  U.initDatabase conn
  handleRequests conn ss
  N.close ss
