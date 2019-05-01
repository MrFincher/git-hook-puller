{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Prelude hiding (lookup)
import Control.Concurrent 
import Control.Lens
import Control.Monad.Reader
import Crypto.HMAC
import Data.Digest.Pure.SHA
import Data.HashMap.Strict hiding (map,filter)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.Text as T (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Foldable
import Data.Semigroup
import Data.Maybe
import Data.Time.Clock
import qualified Data.Yaml as Y
import Network.HTTP.Types.Status
import System.Process
import System.IO
import Web.Scotty.Trans

import Debug.Trace

data Config = Config {port :: Int
                     ,secret :: String
                     ,repos :: HashMap Text RepoEntry}
                     deriving (Generic, Show)

instance FromJSON Config

data RepoEntry = RepoEntry {event :: Text -- only allow valid ones?
                           ,path :: FilePath
                           ,setupCmd :: Maybe Text}
                           deriving (Generic, Show)

instance FromJSON RepoEntry

type Handler = ActionT LT.Text (ReaderT Config IO)

configFile :: FilePath
configFile = "config.yaml"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cfg <- either (error . show) return =<< Y.decodeFileEither configFile
  -- TODO SSL
  scottyT (port cfg) (`runReaderT` cfg) $ post "" hookPosHandler

hookPosHandler :: Handler ()
hookPosHandler = do
  printLogSeperator
  checkHash
  json <- jsonData :: Handler Object
  catchTest json
  RepoEntry {..} <- getConfigEntry json
  checkRelevance event json
  execCmd path "git pull"
  traverse_ (execCmd path) setupCmd

catchTest :: Object -> Handler ()
catchTest json = when ("zen" `member` json) $ text "detected test" >> next

checkRelevance :: Text -> Object -> Handler ()
checkRelevance event json = do
  let relevant = event == "release" && json ^?! ix "action" == "pubished"
               || event == "push" && "pusher" `member` json
  unless relevant $ putStrLnIO "not relevant" >> next

getConfigEntry :: Object -> Handler RepoEntry
getConfigEntry json = do
  let repoJson = json ! "repository" ^?! _Object
  let repoName = repoJson ^?! at "name" . _Just . _String
  maybeEntry <- lift $ asks $ lookup repoName . repos
  maybe (badReq "unknown repo") return maybeEntry 

printLogSeperator :: Handler ()
printLogSeperator = liftIO $ do
    putStrLn "\n\n"
    print =<< getCurrentTime
    putStrLn "handling post request"
    
checkHash :: Handler ()
checkHash = do
  hash <- fromJust <$> header "X-Hub-Signature"
  secret <- lift $ asks $ LBS.pack . filter (not . isSpace) . secret
  shouldBe <- LT.pack . showDigest . hmacSha1 secret <$> body
  when (hash /= "sha1=" <> shouldBe) $
    status forbidden403 >> putStrLnIO "invalid hash" >> next

execCmd :: FilePath -> Text -> Handler ()
execCmd path cmd = liftIO $ do
  T.putStrLn $ "executing: \n " <> cmd 
  let cp = (shell $ T.unpack cmd) {cwd = Just path}
  (exitCode,stout,sterr) <- readCreateProcessWithExitCode cp ""
  print exitCode >> putStrLn stout >> putStrLn sterr
    
badReq :: LT.Text -> Handler a
badReq msg = do
  liftM2 (>>) (liftIO . LT.putStrLn) text msg
  status badRequest400 >> next

putStrLnIO :: Text -> Handler ()
putStrLnIO = liftIO . T.putStrLn