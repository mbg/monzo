--------------------------------------------------------------------------------
-- Haskell bindings for the Monzo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Monzo.MonzoSpec where

--------------------------------------------------------------------------------

import Control.Arrow (left)

import Test.Hspec

import Monzo
import Monzo.Server

--------------------------------------------------------------------------------

monzoTest baseUrl m = left show <$> withMonzoAt baseUrl "" m

spec :: Spec
spec = describe "Monzo" $ beforeAll (startWaiApp server) $ afterAll endWaiApp $ do

    it "Monzo.listAccounts" $ \(_, baseUrl) ->
        monzoTest baseUrl listAccounts `shouldReturn` Right [acc]

    it "Monzo.getBalance" $ \(_, baseUrl) ->
        monzoTest baseUrl (getBalance acc) `shouldReturn` Right balance

--------------------------------------------------------------------------------
