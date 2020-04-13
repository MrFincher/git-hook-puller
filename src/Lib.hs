{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where
import Prelude hiding (log)
import Control.Monad.Reader
import Control.Monad.Except
import Crypto.HMAC
import Data.Digest.Pure.SHA
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text hiding (filter)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types.Status
import qualified Web.Scotty as Scotty

run = Scotty.scotty 8080 $ Scotty.post "" $ undefined

data Config = Config
    { port :: Int
    , secret :: String
    }

data Request = Request
    { headers :: Map Text Text
    , json :: Object
    , body :: LBS.ByteString
    }

data Error = Forbidden Text

class
    ( Loging m
    , CommandRunner m
    , Git m
    , AccesConrol m
    , MonadReader Config m
    , RequestReader m
    )
    => App m where

handle :: App m => m ()
handle = do
    checkHash

checkHash :: App m => m ()
checkHash = do
  hash <- fromJust <$> getHeader "X-Hub-Signature"
  secret <- asks $ LBS.pack . filter (not . isSpace) . secret
  shouldBe <- LT.pack . showDigest . hmacSha1 secret <$> requestBody
  when (hash /= "sha1=" <> shouldBe) $
    liftM2 (>>) forbidden log $ "invalidd hash"

class Loging m where
    log :: Text -> m ()

class CommandRunner m where
    runCmd :: FilePath -> Text -> m ()

class Git m where
    gitPull :: m ()

class AccesConrol m where
    forbidden :: Text -> m ()

class RequestReader m where
    getHeader :: Text -> m (Maybe LT.Text)
    requestBody :: m (LBS.ByteString)

newtype Production a 
    = Production {runProd :: ExceptT Error IO a}
    deriving (Functor, Applicative, Monad
        ,MonadError Error)

instance AccesConrol Production where
    forbidden = throwError . Forbidden