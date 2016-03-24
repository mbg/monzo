--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Mondo (
    module Mondo.Types,
    module Mondo.API
) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class

import qualified Data.ByteString.Internal as BS

import Web.Authenticate.OAuth

import Servant.API.BasicAuth
import Servant.Client (ServantError)

import Mondo.Types
import Mondo.API

--------------------------------------------------------------------------------

mondoAuth :: OAuth
mondoAuth = newOAuth {
    oauthServerName = "https://auth.getmondo.co.uk/"
}

--------------------------------------------------------------------------------
