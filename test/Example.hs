--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Example where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class

import System.IO.Unsafe

import Mondo

--------------------------------------------------------------------------------

token :: String
token = head $ lines $ unsafePerformIO $ readFile "test/token.txt"

--------------------------------------------------------------------------------

foo :: Mondo ()
foo = do
    AccountsResponse [acc] <- getAccounts
    liftIO $ print acc

    r2 <- getBalance (accountID acc)
    liftIO $ print r2

    r3 <- listTransactions (accountID acc)
    liftIO $ print r3

    createFeedItem $ FeedItem
        (accountID acc)
        BasicItem
        (newBasicFeedItem "Haskell test" "https://www.haskell.org/static/img/logo.png?etag=rJR84DMh")
        Nothing

--------------------------------------------------------------------------------
