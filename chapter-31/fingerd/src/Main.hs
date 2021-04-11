{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.Text as T
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as S
import Text.RawString.QQ
import Database.SQLite.Simple.Types
import Data.Maybe (isNothing)
import Control.Monad (when, forever)
import Network.Socket
import qualified Network.Socket as N
import Network.Socket.ByteString
import Text.Parsec
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.List (intersperse)
import qualified Data.ByteString as BS
import Text.Parsec.Text

-- I will `NOTE` things that are different from the book

-- NOTE: userId is a Maybe to be able to create a User without having
-- a ID that comes from a database, with this we don't need the
-- `type UserRow = (Null, Text, Text, Text, Text, Text)`
-- from the book
data User = User { userId :: Maybe Integer
                 , username :: T.Text
                 , shell :: T.Text
                 , homeDirectory :: T.Text
                 , realName :: T.Text
                 , phone :: T.Text
                 } deriving (Eq, Show)


instance FromRow User where
  fromRow = User <$> (Just <$> field)
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User (Just userId_) username_ shell_ homeDirectory_ realName_ phone_) =
    toRow (userId_, username_, shell_, homeDirectory_, realName_, phone_)
  toRow (User Nothing username_ shell_ homeDirectory_ realName_ phone_) =
    toRow (Null, username_, shell_, homeDirectory_, realName_, phone_)

createUsersQ :: Query
createUsersQ = [r|
CREATE TABLE IF NOT EXISTS users
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , username TEXT UNIQUE
  , shell TEXT
  , homeDirectory TEXT
  , realName TEXT
  , phone TEXT
  )
|]

insertUserQ :: Query
insertUserQ = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsersQ :: Query
allUsersQ = "SELECT * FROM users"

userByUsernameQ :: Query
userByUsernameQ = "SELECT * FROM users WHERE username = ?"

userByUsername :: Connection -> T.Text -> IO (Maybe User)
userByUsername conn username_ = do
  results <- query conn userByUsernameQ (Only username_)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> undefined -- TODO: replace with proper exception

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsersQ
  alreadyThere <- userByUsername conn (username me)
  -- NOTE: idempotency, insert an user if not present
  when (isNothing alreadyThere) $ execute conn insertUserQ me
  rows <- query_ conn allUsersQ
  mapM_ print (rows :: [User])
  S.close conn
  where me = User Nothing
                  "coder"
                  "/bin/bash"
                  "/home/coder"
                  "Gabriele Lana"
                  "XXX-YYY-ZZZ"

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

main :: IO ()
main = withSocketsDo $ do
  addressInfo <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing
                 (Just "79")
  ss <- socket (addrFamily $ head addressInfo) Stream defaultProtocol
  N.bind ss (addrAddress $ head addressInfo)
  listen ss 1
  conn <- open "finger.db"
  handleRequests conn ss
  N.close ss
