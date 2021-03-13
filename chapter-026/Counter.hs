{-# LANGUAGE OverloadedStrings #-}
module Counter where

import Web.Scotty.Trans
import Data.Text.Lazy (Text)
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import System.Environment (getArgs)
import Control.Monad.Trans (lift, MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Web.Scotty.Internal.Types (ActionT(ActionT))

data Config = Config { counts :: IORef (M.Map Text Integer)
                     , prefix :: Text
                     }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

hit :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
hit k m = let v = fromMaybe 0 $ M.lookup k m
          in (M.insert k (v + 1) m, v + 1)

app :: Scotty ()
app =
  get "/:key" $ do
    k <- param "key"
    p <- lift $ asks prefix
    let kp = k <> p
    m <- lift $ asks counts
    n <- lift . lift $ atomicModifyIORef m (hit kp)
    html $
      mconcat [ "<h1>Success! Count was: "
              , T.pack $ show (n :: Integer) , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config { counts = counter
                      , prefix = T.pack prefixArg
                      }
      -- XXX: need to review
      -- runR :: ReaderT Config IO Response -> IO Response
      runR (ReaderT cIOr) = cIOr config
  scottyT 3000 runR app
