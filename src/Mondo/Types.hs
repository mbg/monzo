--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Mondo.Types where

--------------------------------------------------------------------------------

import GHC.Generics

import Control.Monad (mzero)

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid ((<>))

import Servant.API

--------------------------------------------------------------------------------

data Empty = Empty

instance FromJSON Empty where
    parseJSON val
        | val == emptyObject = pure Empty
        | otherwise          = mzero

toUnit :: Empty -> ()
toUnit _ = ()

-- | The type of account IDs.
type AccountID     = String

-- | The type of transaction IDs.
type TransactionID = String

-- | The type of webhook IDs.
type WebhookID = String

-- | The type of attachment IDs.
newtype AttachmentID = AttID T.Text
    deriving Show

instance FromJSON AttachmentID where
    parseJSON (String v) = pure $ AttID v
    parseJSON _          = mzero

instance ToFormUrlEncoded AttachmentID where
    toFormUrlEncoded (AttID v) = [("id", v)]

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

--------------------------------------------------------------------------------

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
    parseJSON _ = mzero

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
    parseJSON _ = mzero

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
    transactionID             :: TransactionID,
    transactionDeclineReason  :: Maybe DeclineReason,
    transactionIsLoad         :: Bool,
    transactionSettled        :: Bool,
    transactionCategory       :: Maybe String,
    transactionMerchant       :: Merchant,
    transactionMetadata       :: M.Map String String
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
                    <*> v .: "metadata"
    parseJSON _ = mzero

data Metadata = Metadata { metadata :: M.Map String String }

instance ToFormUrlEncoded Metadata where
    toFormUrlEncoded (Metadata d) =
        [("metadata[" <> T.pack k <> "]", T.pack v) | (k,v) <- M.toList d]

--------------------------------------------------------------------------------

data FeedItemType
    = BasicItem

instance Show FeedItemType where
    show BasicItem = "basic"

instance ToJSON FeedItemType where
    toJSON BasicItem = String "basic"

data FeedItemParams
    = BasicFeedItem {
        itemTitle            :: String,
        itemImageURL         :: String,
        itemBody             :: Maybe String,
        itemBackgroundColour :: Maybe String,
        itemTitleColour      :: Maybe String,
        itemBodyColour       :: Maybe String
    }

newBasicFeedItem :: String -> String -> FeedItemParams
newBasicFeedItem title url = BasicFeedItem {
    itemTitle = title,
    itemImageURL = url,
    itemBody = Nothing,
    itemBackgroundColour = Nothing,
    itemTitleColour = Nothing,
    itemBodyColour = Nothing
}

instance ToFormUrlEncoded FeedItemParams where
    toFormUrlEncoded (BasicFeedItem {..}) =
        [ ( "params[title]"    , T.pack itemTitle    )
        , ( "params[image_url]", T.pack itemImageURL )
        ]

data FeedItem = FeedItem {
    itemAccountID :: AccountID,
    itemType      :: FeedItemType,
    itemParams    :: FeedItemParams,
    itemURL       :: Maybe String
}

instance ToFormUrlEncoded FeedItem where
    toFormUrlEncoded item =
        [ ( "account_id", T.pack $ itemAccountID item   )
        , ( "type"      , T.pack $ show $ itemType item )
        ] ++ toFormUrlEncoded (itemParams item)

--------------------------------------------------------------------------------

data Webhooks = Webhooks {
    webhooks :: [Webhook]
} deriving (Show, Generic)

instance FromJSON Webhooks

data Webhook = Webhook {
    webhookAccountID :: AccountID,
    webhookURL       :: String,
    webhookID        :: Maybe WebhookID
} deriving Show

instance FromJSON Webhook where
    parseJSON (Object v) =
        Webhook <$> v .: "account_id"
                <*> v .: "url"
                <*> v .: "id"
    parseJSON _ = mzero

instance ToFormUrlEncoded Webhook where
    toFormUrlEncoded hook =
        [ ( "account_id", T.pack $ webhookAccountID hook )
        , ( "url"       , T.pack $ webhookURL hook       )
        ]

--------------------------------------------------------------------------------

data FileUploadReq = FileUploadReq {
    uploadFileName :: String,
    uploadFileType :: String
}

instance ToFormUrlEncoded FileUploadReq where
    toFormUrlEncoded req =
        [ ( "file_name", T.pack $ uploadFileName req )
        , ( "file_type", T.pack $ uploadFileType req )
        ]

data FileUploadRes = FileUploadRes {
    uploadURL     :: String,
    uploadPostURL :: String
}

instance FromJSON FileUploadRes where
    parseJSON (Object v) =
        FileUploadRes <$> v .: "file_url"
                      <*> v .: "file_type"
    parseJSON _ = mzero

--------------------------------------------------------------------------------

-- | Transaction attachments.
data Attachment = Attachment {
    attachmentTransaction :: TransactionID,
    attachmentFileType    :: String,
    attachmentURL         :: String,
    attachmentID          :: Maybe AttachmentID,
    attachmentUserID      :: Maybe String,
    attachmentCreated     :: Maybe String
} deriving Show

instance FromJSON Attachment where
    parseJSON (Object v) =
        Attachment <$> v .: "external_id"
                   <*> v .: "file_type"
                   <*> v .: "file_url"
                   <*> v .: "id"
                   <*> v .: "user_id"
                   <*> v .: "created"
    parseJSON _ = mzero

instance ToFormUrlEncoded Attachment where
    toFormUrlEncoded att =
        [ ( "external_id", T.pack $ attachmentTransaction att )
        , ( "file_url",    T.pack $ attachmentURL att         )
        , ( "file_type",   T.pack $ attachmentFileType att    )
        ]

--------------------------------------------------------------------------------
