{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified System.Random as SR
import qualified Database.Redis as R
import Control.Monad (replicateM)
import qualified Data.ByteString as BC
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.URI (URI, parseURI)
import Web.Scotty ( get, param, post, scotty, text, ScottyM )

data WriteError = RedisError R.Reply | TokenAlreadyUsed deriving (Eq, Show)

alphabet :: String
alphabet = ['A'..'Z'] ++ ['0'..'9']

tokenLength :: Int
tokenLength = 1

randomElement :: String -> IO Char
randomElement xs = do
  randomDigit <- SR.randomRIO (0, length xs - 1)
  return $ alphabet !! randomDigit

randomToken :: IO [Char]
randomToken = replicateM tokenLength $ randomElement alphabet

isTaken :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply Bool)
isTaken conn shortURI =
  R.runRedis conn (R.exists shortURI)

writeURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either WriteError R.Status)
writeURI conn shortURI longURI = do
  t <- isTaken conn shortURI
  case t of
    Left r ->
      return $ Left (RedisError r)
    Right True ->
      return $ Left TokenAlreadyUsed
    Right False ->
      R.runRedis conn (R.set shortURI longURI) >>=
      (\case
          Left l -> return $ Left (RedisError l)
          Right r -> return $ Right r)

readURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply (Maybe BC.ByteString))
readURI conn shortURI =
  R.runRedis conn $ R.get shortURI

fromStringToByteString :: String -> BC.ByteString
fromStringToByteString = fromTextToByteString . TL.pack

fromTextToByteString :: TL.Text -> BC.ByteString
fromTextToByteString = encodeUtf8 . TL.toStrict

fromByteStringToText :: BC.ByteString -> TL.Text
fromByteStringToText = TL.fromStrict . decodeUtf8

app :: R.Connection -> ScottyM ()
app conn = do

  get "/ping" $ do
    text "pong"

  post "/short" $ do
    longURI <- param "uri"
    let parsedURI :: Maybe URI
        parsedURI = parseURI (TL.unpack longURI)
    case parsedURI of
      Just _ -> do
        shortToken <- liftIO randomToken
        let bsShortToken = fromStringToByteString shortToken
            bsLongURI = fromTextToByteString longURI
        res <- liftIO (writeURI conn bsShortToken bsLongURI)
        case res of
          Left TokenAlreadyUsed ->
            text $ TL.pack $ "Unfortunately token " <> shortToken <> " was already taken, try again..."
          Left (RedisError r) ->
            text $ TL.pack $ "Redis error " <> show r
          Right _ ->
            text $ TL.pack $ "Your short URL is: http://localhost:3000/long-of/" <> shortToken
      Nothing ->
        text $ TL.pack $ "The given long URL `" <> TL.unpack longURI <> "` is not a valid URL, try again..."

  get "/long-of/:short" $ do
    shortToken <- param "short"
    maybeLongURI <- liftIO (readURI conn shortToken)
    case maybeLongURI of
      Left reply ->
        text $ TL.pack $ "Redis error " <> show reply
      Right Nothing ->
        text $ "Short URI `" <> fromByteStringToText shortToken <> "` not found"
      Right (Just longURI) ->
        text $ "Your long URL is: " <> fromByteStringToText longURI

main :: IO ()
main = do
  conn <- R.connect R.defaultConnectInfo
  scotty 3000 $ app conn
