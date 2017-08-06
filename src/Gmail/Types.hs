{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gmail.Types where

import Data.Char
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.TH

data GmailError = GmailError { gmailError :: GmailErrors }

data GmailErrors = GmailErrors { gmailErrors :: [GoogleError]
                               , gmailCode :: Int
                               , gmailMessage :: String
                               }

data GoogleError = GoogleError { googleDomain :: String
                               , googleReason :: String
                               , googleMessage :: String
                               , googleLocation :: String
                               , googleLocationType :: String
                               }

data OAuth2Installation = OAuth2Installation { oauth2installed :: OAuth2Info }

data OAuth2Info = OAuth2Info { oauth2Client_Id :: String
                             , oauth2Auth_Uri :: String
                             , oauth2Token_Uri :: String
                             , oauth2Auth_Provider_x509_Cert_Url :: String
                             , oauth2Client_Secret :: String
                             , oauth2Redirect_Uris :: [String]
                             }

data OAuth2Tokens = OAuth2Tokens { oauth2Access_Token  :: String
                                 , oauth2Refresh_Token :: String
                                 }

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 5, constructorTagModifier = map toLower} ''GmailError)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 5, constructorTagModifier = map toLower} ''GmailErrors)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 6, constructorTagModifier = map toLower} ''GoogleError)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 6, constructorTagModifier = map toLower} ''OAuth2Installation)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 6, constructorTagModifier = map toLower} ''OAuth2Info)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 6, constructorTagModifier = map toLower} ''OAuth2Tokens)

type AccessToken = String

type TimeStamp = String
type MailAddress = String
data Mail = Mail { mailTo :: [MailAddress]
                 , mailFrom :: MailAddress
                 , mailTime :: TimeStamp
                 , mailBody :: T.Text
                 }

data MailRequest = MailRequest { includeSpamTrash :: Bool
                               , labelIds :: [T.Text]
                               , maxResults :: Int
                               , pageToken :: T.Text
                               , q :: T.Text
                               }

type UrlArguments = [(String, Maybe String)]
