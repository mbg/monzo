--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Mondo (
    module Mondo.Types,

    Mondo,

    withMondo,
    withMondoAt,
    
    listAccounts,
    getBalance,
    getTransaction,
    listTransactions,
    annotateTransaction,
    createFeedItem,
    registerWebhook,
    listWebhooks,
    deleteWebhook,
    uploadAttachment,
    registerAttachment,
    removeAttachment
) where

--------------------------------------------------------------------------------

import Web.Authenticate.OAuth

import Mondo.Types
import Mondo.API

--------------------------------------------------------------------------------

mondoAuth :: OAuth
mondoAuth = newOAuth {
    oauthServerName = "https://auth.getmondo.co.uk/"
}

--------------------------------------------------------------------------------
