{-# LANGUAGE OverloadedStrings #-}

module Gmail.Api.List where

import Gmail.Api
import Gmail.Types
import Gmail.Auth

getMail :: MailRequest -> IO [Mail]
getMail req = do
  tok <- defaultGetToken
  
  
  
