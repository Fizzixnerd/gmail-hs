{-# LANGUAGE OverloadedStrings #-}

module Gmail.Auth where

import Gmail.Types

import qualified System.IO as IO
import Control.Monad (unless)
import System.Directory
import System.Info (os)
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import Data.Char
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Google.OAuth2

oAuth2InfoToOAuth2Client :: OAuth2Info -> OAuth2Client
oAuth2InfoToOAuth2Client info = 
  OAuth2Client { clientId = oauth2Client_Id info
               , clientSecret = oauth2Client_Secret info
               }

getOAuth2Client :: String -> IO OAuth2Client
getOAuth2Client filepath =
  do
    stringRep <- BS.readFile filepath
    case decode stringRep :: Maybe OAuth2Installation of
      Just oauth2installation -> return $ oAuth2InfoToOAuth2Client $ oauth2installed oauth2installation
      otherwise -> error "WTF"

defaultDataDir :: IO FilePath
defaultDataDir = getAppUserDataDirectory "gmail/"

defaultTokenFilePath :: IO FilePath
defaultTokenFilePath = do
  dataDir <- defaultDataDir
  return (dataDir ++ "tokens.json")

defaultOAuth2JsonPath :: IO FilePath
defaultOAuth2JsonPath = do
  dataDir <- defaultDataDir
  return (dataDir ++ "oauth2.json")

defaultOAuth2Client :: IO OAuth2Client
defaultOAuth2Client = do
  path <- defaultOAuth2JsonPath
  getOAuth2Client path

defaultManager :: IO Manager
defaultManager = newManager tlsManagerSettings

defaultGetToken :: IO AccessToken
defaultGetToken = do
  man <- defaultManager
  client <- defaultOAuth2Client
  tokensFile <- defaultTokenFilePath
  exists <- doesFileExist tokensFile
  let permissionUrl = formUrl client ["https://mail.google.com/"]
  unless exists $ do
    putStrLn $ "Load this URL: " ++ show permissionUrl
    case os of
      "linux"  -> rawSystem "xdg-open" [permissionUrl]
      "darwin" -> rawSystem "open"     [permissionUrl]
      _        -> return ExitSuccess
    putStrLn "Please paste the verification code: "
    authcode <- getLine
    tokens   <- exchangeCode client authcode
    putStrLn $ "Received access token: " ++  show (accessToken tokens)
    tokens2 <- refreshTokens client tokens
    putStrLn $ "Received refresh token: " ++ show (accessToken tokens2)
    writeFile tokensFile (show tokens2)
  accessTok <- fmap (accessToken . read) (readFile tokensFile)
  return accessTok
