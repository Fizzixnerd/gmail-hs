{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gmail.TH.Instances where

import Gmail.Types
import Gmail.TH

$(deriveUrlEncode ''MailRequest)

-- something = toUrlArguments $ MailRequest { includeSpamTrash=False, labelIds = ["Inbox"], maxResults = 10, pageToken = "100fdjasked", q = "hello world" }

