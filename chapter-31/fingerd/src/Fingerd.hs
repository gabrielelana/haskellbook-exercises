{-# LANGUAGE OverloadedStrings #-}
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
import User ( User(username, realName, homeDirectory, shell)
            , allUsersQ
            , userByUsername, connect, initDatabase
            )
import Control.Monad (forever)

-- I will `NOTE` things that are different from the book

------------------------------------------------------------------------
-- Finger protocol
------------------------------------------------------------------------

-- NOTE: representing request and response types will let us separate:
-- parsing, handling requests and representing responses. I left the
-- organization of the code logically untouched (aka passing the
-- database connection and the socket client to all the needed
-- functions)
type Host = T.Text
type Username = T.Text
data FingerRequest = ListAllUsersRequest (Maybe Host)
                   | AboutUserRequest Username (Maybe Host)
                   deriving (Eq, Show)

data FingerResponse = ListAllUsersResponse [User]
                    | AboutUserResponse (Maybe User)
                    deriving (Eq, Show)

-- NOTE: I thought that a real parser will be nice to use in a final
-- project since there's a chapter on it
-- TODO: handle verbose
parseRequest :: T.Text -> Either ParseError FingerRequest
parseRequest = parse p "fingerd"
  where p :: Parser FingerRequest
        p = verboseP *> commandP <* crlf
        commandP = AboutUserRequest <$> usernameP <*> optionMaybe hostnameP
               <|> ListAllUsersRequest <$> optionMaybe hostnameP
        usernameP = T.pack <$> many1 alphaNum
        hostnameP = string "@" *> (T.pack <$> many1 alphaNum)
        verboseP = option "" (string "/W" <* many (char ' '))

handleRequest :: Connection -> Socket -> FingerRequest -> IO ()
handleRequest db sc (ListAllUsersRequest _) = do
  users <- query_ db allUsersQ
  renderResponse sc (ListAllUsersResponse users)
handleRequest db sc (AboutUserRequest u _) =
  userByUsername db u >>= renderResponse sc . AboutUserResponse

renderResponse :: Socket -> FingerResponse -> IO ()
renderResponse sc (ListAllUsersResponse users) =
  sendAll sc (encodeUtf8 response)
  where response = T.concat $ intersperse "\n" $ username <$> users
renderResponse sc (AboutUserResponse (Just user)) =
  -- TODO: something better
  sendAll sc $ BS.concat [ "Login: ", e $ username user, "\t\t"
                         , "Name: ", e $ realName user, "\n"
                         , "Directory: ", e $ homeDirectory user, "\t\t"
                         , "Shell: ", e $ shell user, "\n"]
  where e = encodeUtf8
renderResponse sc (AboutUserResponse Nothing) =
  -- TODO: something better
  sendAll sc "ERROR: user not found"

-- TODO: something better
handleError :: Socket -> IO ()
handleError sc = sendAll sc "ERROR: unknown finger command"

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
  conn <- User.connect
  initDatabase conn
  handleRequests conn ss
  N.close ss
