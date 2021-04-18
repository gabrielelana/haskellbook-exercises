{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module User where

import qualified Data.Text as T
import Database.SQLite.Simple
import Text.RawString.QQ
import Database.SQLite.Simple.Types
import Data.Maybe (isNothing)
import Control.Monad (when)

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

------------------------------------------------------------------------
-- SQLite
------------------------------------------------------------------------

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

updateUserQ :: Query
updateUserQ = [r|
UPDATE users
SET username = :username
  , shell = :shell
  , homeDirectory = :homeDirectory
  , realName = :realName
  , phone = :phone
WHERE id = :userId
|]

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

insertUser :: Connection -> User -> IO ()
insertUser conn = execute conn insertUserQ

updateUser :: Connection -> User -> IO ()
updateUser conn u = executeNamed conn updateUserQ [ ":userId" := userId u
                                                  , ":username" := username u
                                                  , ":shell" := shell u
                                                  , ":homeDirectory" := homeDirectory u
                                                  , ":realName" := realName u
                                                  , ":phone" := phone u
                                                  ]

connect :: IO Connection
connect = open "finger.db"

initDatabase :: Connection -> IO ()
initDatabase conn = do
  execute_ conn createUsersQ
  alreadyThere <- userByUsername conn (username me)
  -- NOTE: idempotency, insert an user if not present
  when (isNothing alreadyThere) $ execute conn insertUserQ me
  rows <- query_ conn allUsersQ
  mapM_ print (rows :: [User])
  where me = User Nothing
                  "coder"
                  "/bin/bash"
                  "/home/coder"
                  "Gabriele Lana"
                  "XXX-YYY-ZZZ"
