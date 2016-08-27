--------------------------------------------------------------------------------
-- Haskell bindings for the Monzo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Example where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.IO.Class

import System.IO.Unsafe

import Monzo

--------------------------------------------------------------------------------

token :: String
token = head $ lines $ unsafePerformIO $ readFile "token.txt"

--------------------------------------------------------------------------------

printAccount :: Account -> Monzo ()
printAccount Account{..} = liftIO $ do
    putStr accountDescription
    putStrLn $ " (" ++ accountID ++ ")"
    putStr "Created: "
    print (timestamp accountCreated)

printBalance :: Balance -> Monzo ()
printBalance Balance{..} = liftIO $ do
    putStr "Balance: "
    print balanceValue
    putStr "Currency: "
    putStrLn balanceCurrency
    putStr "Spent today: "
    print balanceSpentToday

printTransaction :: Transaction -> Monzo ()
printTransaction t@Transaction{..} = liftIO $ do
    print t

foo :: Monzo ()
foo = do
    -- get a list of accounts; this always returns one account at the moment
    [acc] <- listAccounts
    printAccount acc

    -- get the current balance of the account
    b <- getBalance acc
    printBalance b

    -- get up to 100 transactions
    ts <- listTransactions acc defaultPageOptions

    -- retrieve more information for each transaction
    forM_ ts $ \t -> do
        ft <- getTransaction t True
        printTransaction ft

    {-createFeedItem $ FeedItem
        (accountID acc)
        BasicItem
        (newBasicFeedItem "Haskell test" "https://www.haskell.org/static/img/logo.png?etag=rJR84DMh")
        Nothing-}

--------------------------------------------------------------------------------
