{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO                        (stdout,stdin)
import Control.Monad.IO.Class           (liftIO)
import qualified Web.Authenticate.OAuth as OA
import qualified Data.Conduit.List      as CL
import qualified Data.ByteString        as S
import qualified Data.Conduit.Binary    as CB
import qualified Network.HTTP.Simple    as HS
import qualified Network.HTTP.Client    as HC

oauth = OA.newOAuth {
    OA.oauthRequestUri      = "https://api.twitter.com/oauth/request_token",
    OA.oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token",
    OA.oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize",
    OA.oauthConsumerKey     = "your consumer key here",
    OA.oauthConsumerSecret  = "your consumer secret here"
}

credential = OA.newCredential "your access token" "your access token secret"

main :: IO ()
main = do
    S.hPutStr stdout "Enter some keyword to search in tweets: "
    key <- S.hGetLine stdin
    manager <- HC.newManager HC.defaultManagerSettings
    request <- HC.parseRequest "GET https://stream.twitter.com/1.1/statuses/filter.json"
    let req = HC.setQueryString [("track", Just key)] request
    signedRequest <- OA.signOAuth oauth credential req
    HS.httpSink signedRequest $ \res -> do
        liftIO $ Prelude.putStrLn $ "Status code:" ++ show (HS.getResponseStatusCode res)
        CL.mapM_ (S.hPut stdout)

