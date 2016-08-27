--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Monzo (
    module Monzo.Types,

    Monzo,

    withMonzo,
    withMonzoAt,

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

import Monzo.Types
import Monzo.API

--------------------------------------------------------------------------------

monzoAuth :: OAuth
monzoAuth = newOAuth {
    oauthServerName = "https://auth.getmondo.co.uk/"
}

--------------------------------------------------------------------------------
