--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Mondo.Types where

--------------------------------------------------------------------------------

import GHC.Generics

import Control.Monad (mzero)

import Data.Aeson
import Data.Aeson.Types                             (Parser, emptyObject)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Time.RFC3339
import qualified Data.Map as M
import Data.Monoid ((<>))

import Servant.API

--------------------------------------------------------------------------------

-- | Retrieve the value associated with the given key of an `Object`. The result
--   is `Nothing` if the key is not present or if it is null, or `empty` if the
--   value cannot be converted to the desired type.
(.:??) :: FromJSON a => Object -> T.Text -> Parser (Maybe a)
obj .:?? key = case H.lookup key obj of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just val -> parseJSON val

--------------------------------------------------------------------------------

lookupEither :: (Show a, Eq a) => a -> [(a,b)] -> Either String b
lookupEither x xs = maybe (Left $ "could not find key " <> show x) return (lookup x xs)

eitherOr :: Eq a => a -> [(a,b)] -> Either String b -> Either String b
eitherOr x xs f = maybe f return (lookup x xs)

--------------------------------------------------------------------------------

-- | An abstract representation of timestamps used by the Mondo API.
newtype Timestamp = Timestamp { timestamp :: ZonedTime }
    deriving (Show)

instance Eq Timestamp where
    (Timestamp x) == (Timestamp y) =
        zonedTimeToUTC x == zonedTimeToUTC y

instance ToHttpApiData Timestamp where
    toQueryParam (Timestamp time) = formatTimeRFC3339 time

instance FromHttpApiData Timestamp where
    parseQueryParam v = case parseTimeRFC3339 v of
        Nothing -> Left "Can't parse time"
        Just t  -> Right $ Timestamp t

instance FromJSON Timestamp where
    parseJSON (String v) = case parseTimeRFC3339 v of
        Nothing -> mzero
        Just t  -> pure $ Timestamp t
    parseJSON _ = mzero

instance ToJSON Timestamp where
    toJSON (Timestamp t) = String (formatTimeRFC3339 t)

--------------------------------------------------------------------------------

data Since = SinceTime Timestamp | SinceID String

instance ToHttpApiData Since where
    toQueryParam (SinceTime time) = toQueryParam time
    toQueryParam (SinceID id)     = T.pack id

instance FromHttpApiData Since where
    parseQueryParam v = case parseTimeRFC3339 v of
        Just t  -> Right $ SinceTime $ Timestamp t
        Nothing -> Right $ SinceID $ T.unpack v

--------------------------------------------------------------------------------

data PageOptions = PageOptions {
    limit :: Maybe Int,
    since :: Maybe Since,
    before :: Maybe ZonedTime
}

defaultPageOptions :: PageOptions
defaultPageOptions = PageOptions {
    limit = Just 100,
    since = Nothing,
    before = Nothing
}

--------------------------------------------------------------------------------

data Empty = Empty

instance FromJSON Empty where
    parseJSON val
        | val == emptyObject = pure Empty
        | otherwise          = mzero

instance ToJSON Empty where
    toJSON Empty = object []

toUnit :: Empty -> ()
toUnit _ = ()

--------------------------------------------------------------------------------

-- | The type of account IDs.
type AccountID = String

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

instance ToJSON AttachmentID where
    toJSON (AttID v) = String v

instance ToFormUrlEncoded AttachmentID where
    toFormUrlEncoded (AttID v) = [("id", v)]

instance FromFormUrlEncoded AttachmentID where
    fromFormUrlEncoded dict = AttID <$> lookupEither "id" dict

--------------------------------------------------------------------------------

data AccountsResponse = AccountsResponse {
    accounts :: [Account]
} deriving (Show, Generic)

instance FromJSON AccountsResponse
instance ToJSON AccountsResponse

data Account = Account {
    accountID          :: AccountID,
    accountDescription :: String,
    accountCreated     :: Timestamp
} deriving (Eq, Show, Generic)

instance FromJSON Account where
    parseJSON (Object v) =
        Account <$> v .: "id"
                <*> v .: "description"
                <*> v .: "created"
    parseJSON _ = mzero

instance ToJSON Account where
    toJSON Account{..} =
        object [ "id"          .= accountID
               , "description" .= accountDescription
               , "created"     .= accountCreated
               ]

data Balance = Balance {
    balanceValue      :: Integer,
    balanceCurrency   :: String,
    balanceSpentToday :: Integer
} deriving (Eq, Show, Generic)

instance FromJSON Balance where
    parseJSON (Object v) =
        Balance <$> v .: "balance"
                <*> v .: "currency"
                <*> v .: "spend_today"
    parseJSON _ = mzero

instance ToJSON Balance where
    toJSON Balance{..} =
        object [ "balance"     .= balanceValue
               , "currency"    .= balanceCurrency
               , "spend_today" .= balanceSpentToday
               ]

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

instance ToJSON DeclineReason where
    toJSON InsufficientFunds = String "INSUFFICIENT_FUNDS"
    toJSON CardInactive      = String "CARD_INACTIVE"
    toJSON CardBlocked       = String "CARD_BLOCKED"
    toJSON Other             = String "OTHER"

data Address = Address {
    addrAddress   :: String,
    addrCity      :: String,
    addrCountry   :: String,
    addrLatitude  :: Double,
    addrLongitude :: Double,
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

instance ToJSON Address where
    toJSON Address{..} =
        object [ "address"   .= addrAddress
               , "city"      .= addrCity
               , "country"   .= addrCountry
               , "latitude"  .= addrLatitude
               , "longitude" .= addrLongitude
               , "postcode"  .= addrPostcode
               , "region"    .= addrRegion
               ]

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

newMerchant :: String -> Merchant
newMerchant id = Merchant {
    merchantAddress = Nothing,
    merchantCreated = Nothing,
    merchantGroupID = Nothing,
    merchantID = id,
    merchantLogo = Nothing,
    merchantEmoji = Nothing,
    merchantName = Nothing,
    merchantCategory = Nothing
}

instance FromJSON Merchant where
    parseJSON (Object v) =
        Merchant <$> v .:? "address"
                 <*> v .:? "created"
                 <*> v .:? "group_id"
                 <*> v .: "id"
                 <*> v .:? "logo"
                 <*> v .:? "emoji"
                 <*> v .:? "name"
                 <*> v .:? "category"
    parseJSON (String v) =
        pure (newMerchant $ T.unpack v)
    parseJSON _ =
        fail "Can't parse merchant."

instance ToJSON Merchant where
    toJSON Merchant{..} =
        object [ "address"  .= merchantAddress
               , "created"  .= merchantCreated
               , "group_id" .= merchantGroupID
               , "id"       .= merchantID
               , "logo"     .= merchantLogo
               , "emoji"    .= merchantEmoji
               , "name"     .= merchantName
               , "category" .= merchantCategory
               ]

data Transactions = Transactions {
    transactions :: [Transaction]
} deriving (Show, Generic)

instance FromJSON Transactions
instance ToJSON Transactions

data TransactionResponse = TransactionResponse {
    transaction :: Transaction
} deriving (Show, Generic)

instance FromJSON TransactionResponse
instance ToJSON TransactionResponse

-- TODO: add metadata

data Transaction
    = Transaction {
        transactionAccountBalance :: Integer,
        transactionAmount         :: Integer,
        transactionCreated        :: Timestamp,
        transactionCurrency       :: String,
        transactionDescription    :: String,
        transactionID             :: TransactionID,
        transactionDeclineReason  :: Maybe DeclineReason,
        transactionIsLoad         :: Bool,
        transactionSettled        :: Maybe Timestamp,
        transactionCategory       :: Maybe String,
        transactionMerchant       :: Maybe Merchant,
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
                    <*> v .:? "decline_reason"
                    <*> v .: "is_load"
                    <*> v .:? "settled"
                    <*> v .:? "category"
                    <*> v .:?? "merchant"
                    <*> v .: "metadata"
    parseJSON v = fail "Can't parse transaction."

instance ToJSON Transaction where
    toJSON Transaction{..} =
        object [ "account_balance" .= transactionAccountBalance
               , "amount"          .= transactionAmount
               , "created"         .= transactionCreated
               , "currency"        .= transactionCurrency
               , "description"     .= transactionDescription
               , "id"              .= transactionID
               , "decline_reason"  .= transactionDeclineReason
               , "is_load"         .= transactionIsLoad
               , "settled"         .= transactionSettled
               , "category"        .= transactionSettled
               , "merchant"        .= transactionMerchant
               , "metadata"        .= transactionMetadata
               ]

data Metadata = Metadata { metadata :: M.Map String String }

instance ToFormUrlEncoded Metadata where
    toFormUrlEncoded (Metadata d) =
        [("metadata[" <> T.pack k <> "]", T.pack v) | (k,v) <- M.toList d]

instance FromFormUrlEncoded Metadata where
    fromFormUrlEncoded dict =
        return $ Metadata $ M.fromList [(T.unpack k, T.unpack v) | (k,v) <- dict]

--------------------------------------------------------------------------------

data FeedItemType
    = BasicItem

instance Show FeedItemType where
    show BasicItem = "basic"

instance Read FeedItemType where
    readsPrec _ "basic" = [(BasicItem, "")]

instance ToJSON FeedItemType where
    toJSON BasicItem = String "basic"

data FeedItemParams (k :: FeedItemType)
    = BasicFeedItem {
        itemTitle            :: String,
        itemImageURL         :: String,
        itemBody             :: Maybe String,
        itemBackgroundColour :: Maybe String,
        itemTitleColour      :: Maybe String,
        itemBodyColour       :: Maybe String
    }

newBasicFeedItem :: String -> String -> FeedItemParams BasicItem
newBasicFeedItem title url = BasicFeedItem {
    itemTitle = title,
    itemImageURL = url,
    itemBody = Nothing,
    itemBackgroundColour = Nothing,
    itemTitleColour = Nothing,
    itemBodyColour = Nothing
}

instance ToFormUrlEncoded (FeedItemParams k) where
    toFormUrlEncoded BasicFeedItem{..} =
        [ ( "params[title]"    , T.pack itemTitle    )
        , ( "params[image_url]", T.pack itemImageURL )
        ]

instance FromFormUrlEncoded (FeedItemParams k) where
    fromFormUrlEncoded dict = do
        title    <- lookupEither "params[title]" dict
        imageURL <- lookupEither "params[image_url]" dict

        return BasicFeedItem {
            itemTitle = T.unpack title,
            itemImageURL = T.unpack imageURL,
            itemBody = Nothing,
            itemBackgroundColour = Nothing,
            itemTitleColour = Nothing,
            itemBodyColour = Nothing
        }

data FeedItem (k :: FeedItemType) = FeedItem {
    itemAccountID :: AccountID,
    itemType      :: FeedItemType,
    itemParams    :: FeedItemParams k,
    itemURL       :: Maybe String
}

instance ToFormUrlEncoded (FeedItem k) where
    toFormUrlEncoded item =
        [ ( "account_id", T.pack $ itemAccountID item   )
        , ( "type"      , T.pack $ show $ itemType item )
        ] ++ toFormUrlEncoded (itemParams item)

instance FromFormUrlEncoded (FeedItem k) where
    fromFormUrlEncoded dict = do
        accountID <- lookupEither "account_id" dict
        typ <- lookupEither "type" dict
        params <- fromFormUrlEncoded dict

        return FeedItem {
            itemAccountID = T.unpack accountID,
            itemType = read $ T.unpack typ,
            itemParams = params,
            itemURL = Nothing
        }

--------------------------------------------------------------------------------

data Webhooks = Webhooks {
    webhooks :: [Webhook]
} deriving (Show, Generic)

instance FromJSON Webhooks
instance ToJSON Webhooks

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

instance ToJSON Webhook where
    toJSON Webhook{..} =
        object [ "account_id" .= webhookAccountID
               , "url"        .= webhookURL
               , "id"         .= webhookID
               ]

instance ToFormUrlEncoded Webhook where
    toFormUrlEncoded hook =
        [ ( "account_id", T.pack $ webhookAccountID hook )
        , ( "url"       , T.pack $ webhookURL hook       )
        ]

instance FromFormUrlEncoded Webhook where
    fromFormUrlEncoded dict = do
        acc <- lookupEither "account_id" dict
        url <- lookupEither "url" dict
        return Webhook {
            webhookAccountID = T.unpack acc,
            webhookURL       = T.unpack url,
            webhookID        = Nothing
        }

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

instance FromFormUrlEncoded FileUploadReq where
    fromFormUrlEncoded dict = do
        name <- lookupEither "file_name" dict
        typ  <- lookupEither "file_type" dict
        return $ FileUploadReq (T.unpack name) (T.unpack typ)

data FileUploadRes = FileUploadRes {
    uploadURL     :: String,
    uploadPostURL :: String
}

instance FromJSON FileUploadRes where
    parseJSON (Object v) =
        FileUploadRes <$> v .: "file_url"
                      <*> v .: "upload_url"
    parseJSON _ = mzero

instance ToJSON FileUploadRes where
    toJSON FileUploadRes{..} =
        object [ "file_url"   .= uploadURL
               , "upload_url" .= uploadPostURL
               ]

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

instance ToJSON Attachment where
    toJSON Attachment{..} =
        object [ "external_id" .= attachmentTransaction
               , "file_type"   .= attachmentFileType
               , "file_url"    .= attachmentURL
               , "id"          .= attachmentID
               , "user_id"     .= attachmentUserID
               , "created"     .= attachmentCreated
               ]

instance ToFormUrlEncoded Attachment where
    toFormUrlEncoded att =
        [ ( "external_id", T.pack $ attachmentTransaction att )
        , ( "file_url",    T.pack $ attachmentURL att         )
        , ( "file_type",   T.pack $ attachmentFileType att    )
        ]

instance FromFormUrlEncoded Attachment where
    fromFormUrlEncoded dict = do
        id <- lookupEither "external_id" dict
        url <- lookupEither "file_url" dict
        typ <- lookupEither "file_type" dict

        return Attachment {
            attachmentTransaction = T.unpack id,
            attachmentURL = T.unpack url,
            attachmentFileType = T.unpack typ,
            attachmentID = Nothing,
            attachmentUserID = Nothing,
            attachmentCreated = Nothing
        }

--------------------------------------------------------------------------------
