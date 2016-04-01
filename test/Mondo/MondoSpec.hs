--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Mondo.MondoSpec where

--------------------------------------------------------------------------------

import Control.Arrow (left)

import Test.Hspec

import Mondo
import Mondo.Server

--------------------------------------------------------------------------------

mondoTest baseUrl m = left show <$> withMondoAt baseUrl "" m

spec :: Spec
spec = describe "Mondo" $ beforeAll (startWaiApp server) $ afterAll endWaiApp $ do

    it "Mondo.listAccounts" $ \(_, baseUrl) ->
        mondoTest baseUrl listAccounts `shouldReturn` Right [acc]

    it "Mondo.getBalance" $ \(_, baseUrl) ->
        mondoTest baseUrl (getBalance acc) `shouldReturn` Right balance

--------------------------------------------------------------------------------
