{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO                        (stdout)
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
    OA.oauthConsumerKey     = "your consuer key here",
    OA.oauthConsumerSecret  = "your consumer secret here" 
}

credential = OA.newCredential "your access token" "your access token secret"

main :: IO ()
main = do
    manager <- HC.newManager HC.defaultManagerSettings
    request <- HC.parseRequest "POST https://api.twitter.com/1.1/statuses/update.json"
    let postRequest = HC.urlEncodedBody [("status", "Test from Haskell")] request
    signedRequest <- OA.signOAuth oauth credential postRequest
    HS.httpSink signedRequest $ \res -> do
        liftIO $ Prelude.putStrLn $ "Status code:" ++ show (HS.getResponseStatusCode res)
        CL.mapM_ (S.hPut stdout)

