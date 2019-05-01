{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (lookup)
import Control.Concurrent 
import Control.Lens
import Control.Monad.Reader
import Crypto.HMAC
import Data.Digest.Pure.SHA
import Data.HashMap.Strict hiding (map,filter)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import Data.Text as T (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import Data.Semigroup
import Data.Maybe
import Data.Time.Clock
import Network.HTTP.Types.Status
import System.Process
import System.IO
import Web.Scotty.Trans

import Debug.Trace

data Config = Config {port :: Int
                     ,secret :: String
                     ,repos :: HashMap Text FilePath}

type Handler = ActionT LT.Text (ReaderT Config IO)

readConfig :: IO Config
readConfig = do
  (port:secret:rest) <- lines <$> readFile "config.txt"
  return $ Config (read port) secret $ foldMap (aux . words) rest
  where aux [name, path] = singleton (T.pack name) path

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cfg <- readConfig
  -- TODO SSL
  putStrLn "starting scotty..."
  scottyT (port cfg) (`runReaderT` cfg) $ post "" hookPosHandler

hookPosHandler :: Handler ()
hookPosHandler = do
  printLogSeperator
  checkHash
  json <- jsonData :: Handler Object
  liftIO $ print $ json ^? ix "action"
  path <- getRepoPath $ json ^?! repoNameLens
  when (json ! "action" == "published") $ execGitPull path

getRepoPath :: Text -> Handler FilePath
getRepoPath repoName = lift (asks $ lookup repoName . repos) >>= \case
    Just path -> return path
    Nothing -> do liftIO (putStrLn "unknown repo") >> text "unknown repo"
                  status badRequest400 >> next


printLogSeperator :: Handler ()
printLogSeperator = liftIO $ do
    putStrLn "\n\n"
    print =<< getCurrentTime
    putStrLn "handling post request"
    
checkHash :: Handler ()
checkHash = do
  hash <- fromJust <$> header "X-Hub-Signature"
  secret <- lift $ asks $ BS.pack . filter (not . isSpace) . secret
  shouldBe <- LT.pack . showDigest . hmacSha1 secret <$> body
  when (hash /= "sha1=" <> shouldBe) $
    status forbidden403 >> liftIO (putStrLn "invalid hash") >> next


execGitPull :: FilePath -> Handler ()
execGitPull path = liftIO $ do
  let cp = (shell "git pull") {cwd = Just path}
  (exitCode,stout,sterr) <- readCreateProcessWithExitCode cp ""
  print exitCode >> putStrLn stout >> putStrLn sterr
    

repoNameLens :: Traversal' Object Text
repoNameLens = at "repository" . _Just . _Object
             . at "name" . _Just . _String