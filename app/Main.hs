{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import System.IO                        (stdout,stdin)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad                    (mzero)
import Data.Aeson.Parser                (json)
import Data.Conduit                     (($$))
import Data.Conduit.Attoparsec          (sinkParser)
import qualified Web.Authenticate.OAuth as OA
import qualified Data.Conduit.List      as CL
import qualified Data.ByteString        as S
import qualified Data.Conduit.Binary    as CB
import qualified Network.HTTP.Simple    as HS
import qualified Network.HTTP.Client    as HC
import Network.HTTP.Client.TLS          (tlsManagerSettings)
import Network.HTTP.Client.Conduit      (bodyReaderSource)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TI
import Data.Aeson                       --(FromJSON, Object, (.:), parseJSON)

oauth = OA.newOAuth {
    OA.oauthRequestUri      = "https://api.twitter.com/oauth/request_token",
    OA.oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token",
    OA.oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize",
    OA.oauthConsumerKey     = "your consumer key here",
    OA.oauthConsumerSecret  = "your consumer secret here"
}

credential = OA.newCredential "your access token" "your access token secret"

data User = User { screenName :: T.Text } deriving (Show)
data Tweet = Tweet { text :: T.Text, user :: User } deriving (Show)

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "screen_name"
    parseJSON _ = mzero

instance FromJSON Tweet where
    parseJSON (Object v) = Tweet <$> v .: "text" <*> v .: "user"
    parseJSON _ = mzero

printSink = do
    j <- sinkParser json
    case fromJSON j of
        Success (Tweet {text=t, user=User { screenName=name }}) -> do
            liftIO $ TI.putStrLn $ T.concat [name, ": ", t]
            liftIO $ TI.putStrLn "----------------------------------------"
        Error _ -> liftIO $ return ()
    printSink

main :: IO ()
main = do
    S.hPutStr stdout "Enter some keyword to search in tweets: "
    key <- S.hGetLine stdin
    manager <- HC.newManager tlsManagerSettings
    request <- HC.parseRequest "GET https://stream.twitter.com/1.1/statuses/filter.json"
    let req = HC.setQueryString [("track", Just key)] request
    signedRequest <- OA.signOAuth oauth credential req
    HC.withResponse signedRequest manager $ \res -> do
        Prelude.putStrLn $ "Status code:" ++ show (HS.getResponseStatusCode res)
        bodyReaderSource (HC.responseBody res) $$ printSink

