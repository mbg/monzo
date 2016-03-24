--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Mondo.Types where

--------------------------------------------------------------------------------

import GHC.Generics

import Control.Monad (mzero)

import Data.Aeson

--------------------------------------------------------------------------------

type AccountID = String

--------------------------------------------------------------------------------

data AccountsResponse = AccountsResponse {
    accounts :: [Account]
} deriving (Show, Generic)

instance FromJSON AccountsResponse

data Account = Account {
    id          :: AccountID,
    description :: String,
    created     :: String
} deriving (Show, Generic)

accountID :: Account -> String
accountID (Account x _ _) = x

instance FromJSON Account

data Balance = Balance {
    balance     :: Integer,
    currency    :: String,
    spend_today :: Integer
} deriving (Show, Generic)

instance FromJSON Balance

-- | Enumerates reasons which cause transactions to be declined.
data DeclineReason
    = InsufficientFunds
    | CardInactive
    | CardBlocked
    | Other
    deriving Show

instance FromJSON DeclineReason where
    parseJSON (String "INSUFFICIENT_FUNDS") = pure InsufficientFunds
    parseJSON (String "CARD_INACTIVE")      = pure CardInactive
    parseJSON (String "CARD_BLOCKED")       = pure CardBlocked
    parseJSON (String "OTHER")              = pure Other
    parseJSON _                             = mzero

data Address = Address {
    addrAddress   :: String,
    addrCity      :: String,
    addrCountry   :: String,
    addrLatitude  :: String,
    addrLongitude :: String,
    addrPostcode  :: String,
    addrRegion    :: String
} deriving Show

instance FromJSON Address where
    parseJSON (Object v) =
        Address <$> v .: "address"
                <*> v .: "city"
                <*> v .: "country"
                <*> v .: "latitude"
                <*> v .: "longitude"
                <*> v .: "postcode"
                <*> v .: "region"

data Merchant = Merchant {
    merchantAddress  :: Maybe Address,
    merchantCreated  :: Maybe String,
    merchantGroupID  :: Maybe String,
    merchantID       :: String,
    merchantLogo     :: Maybe String,
    merchantEmoji    :: Maybe String,
    merchantName     :: Maybe String,
    merchantCategory :: Maybe String
} deriving Show

instance FromJSON Merchant where
    parseJSON (Object v) =
        Merchant <$> v .: "address"
                 <*> v .: "created"
                 <*> v .: "group_id"
                 <*> v .: "id"
                 <*> v .: "logo"
                 <*> v .: "emoji"
                 <*> v .: "name"
                 <*> v .: "category"

data Transactions = Transactions {
    transactions :: [Transaction]
} deriving (Show, Generic)

instance FromJSON Transactions

data TransactionResponse = TransactionResponse {
    transaction :: Transaction
} deriving (Show, Generic)

instance FromJSON TransactionResponse

-- TODO: add metadata

data Transaction = Transaction {
    transactionAccountBalance :: Integer,
    transactionAmount         :: Integer,
    transactionCreated        :: String,
    transactionCurrency       :: String,
    transactionDescription    :: String,
    transactionID             :: String,
    transactionDeclineReason  :: Maybe DeclineReason,
    transactionIsLoad         :: Bool,
    transactionSettled        :: Bool,
    transactionCategory       :: Maybe String,
    transactionMerchant       :: Merchant
} deriving Show

instance FromJSON Transaction where
    parseJSON (Object v) =
        Transaction <$> v .: "account_balance"
                    <*> v .: "amount"
                    <*> v .: "created"
                    <*> v .: "currency"
                    <*> v .: "description"
                    <*> v .: "id"
                    <*> v .: "decline_reason"
                    <*> v .: "is_load"
                    <*> v .: "settled"
                    <*> v .: "category"
                    <*> v .: "merchant"

--------------------------------------------------------------------------------
