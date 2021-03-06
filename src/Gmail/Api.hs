{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gmail.Api where

import qualified Data.Text as T

import Gmail.Types

--FIXME: This shouldnt' be hard coded!
defaultGetMailRequest :: IO MailRequest
defaultGetMailRequest = return MailRequest { includeSpamTrash = False
                                           , labelIds = []
                                           , maxResults = 10 
                                           , pageToken = ""
                                           , q = ""
                                           }

newMailQuery :: T.Text
newMailQuery = "rfc822msgid:is:unread"

defaultGetNewMailRequest :: IO MailRequest
defaultGetNewMailRequest = fmap (\req -> req {q = newMailQuery}) defaultGetMailRequest
