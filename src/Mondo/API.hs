--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Mondo.API (
    Mondo,

    withMondo,
    getAccounts,
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

import GHC.TypeLits

import qualified Data.ByteString.Internal as BS
import Data.Proxy
import Data.Monoid ((<>))

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS

import Servant.API hiding (addHeader)
import Servant.Common.Req (Req(..), addHeader)
import Servant.Client

import Mondo.Types

--------------------------------------------------------------------------------

data MondoAuth

instance HasClient api => HasClient (MondoAuth :> api) where
    type Client (MondoAuth :> api) = String -> Client api

    clientWithRoute Proxy req url mgr val =
        clientWithRoute (Proxy :: Proxy api) (mondoAuthReq val req) url mgr

mondoAuthReq :: String -> Req -> Req
mondoAuthReq token = addHeader "Authorization" ("Bearer " <> token)

--------------------------------------------------------------------------------

type TransactionsAPI =
      Capture "transaction_id" TransactionID :>
      QueryParam "expand[]" String :>
      Get '[JSON] TransactionResponse
 :<|> QueryParam "account_id" AccountID :>
      Get '[JSON] Transactions
 :<|> Capture "transaction_id" TransactionID :>
      ReqBody '[FormUrlEncoded] Metadata :>
      Patch '[JSON] TransactionResponse

-- | A type representing Mondo's feed API.
type FeedAPI =
      ReqBody '[FormUrlEncoded] FeedItem :> Post '[JSON] Empty

-- | A type representing Mondo's webhooks API.
type WebhooksAPI =
      ReqBody '[FormUrlEncoded] Webhook :> Post '[JSON] Webhook
 :<|> QueryParam "account_id" AccountID :> Get '[JSON] Webhooks
 :<|> Capture "webhook_id" WebhookID :> Delete '[JSON] Empty

-- | A type representing Mondo's attachments API.
type AttachmentsAPI =
      "upload" :> ReqBody '[FormUrlEncoded] FileUploadReq :> Post '[JSON] FileUploadRes
 :<|> "register" :> ReqBody '[FormUrlEncoded] Attachment :> Post '[JSON] Attachment
 :<|> "deregister" :> ReqBody '[FormUrlEncoded] AttachmentID :> Post '[JSON] Empty

type AuthAPI =
      "accounts" :> Get '[JSON] AccountsResponse
 :<|> "balance" :> QueryParam "account_id" AccountID :> Get '[JSON] Balance
 :<|> "transactions" :> TransactionsAPI
 :<|> "feed" :> FeedAPI
 :<|> "webhooks" :> WebhooksAPI
 :<|> "attachment" :> AttachmentsAPI

-- | A type representing the Mondo API.
type MondoAPI = MondoAuth :> AuthAPI

mondoAPI :: Proxy MondoAPI
mondoAPI = Proxy

-- | The URL of the Mondo API.
mondoURL :: BaseUrl
mondoURL = BaseUrl Https "api.getmondo.co.uk" 443 "/"

--------------------------------------------------------------------------------

type Mondo = ReaderT String (ReaderT Manager (ExceptT ServantError IO))

withMondo :: String -> Mondo a -> IO (Either ServantError a)
withMondo tkn m = do
    manager <- newManager tlsManagerSettings
    runExceptT (runReaderT (runReaderT m tkn) manager)

credential :: Mondo String
credential = ask

manager :: Mondo Manager
manager = lift ask

--------------------------------------------------------------------------------

fullApi = client mondoAPI mondoURL

transactionsApi :: Manager -> String ->
      (TransactionID -> Maybe String -> ExceptT ServantError IO TransactionResponse)
 :<|> ((Maybe AccountID -> ExceptT ServantError IO Transactions)
 :<|> (TransactionID -> Metadata -> ExceptT ServantError IO TransactionResponse))
transactionsApi mgr tkn = getNth (Proxy :: Proxy 2) $ fullApi mgr tkn

feedApi mgr tkn = getNth (Proxy :: Proxy 3) $ fullApi mgr tkn
webhooksApi mgr tkn = getNth (Proxy :: Proxy 4) $ fullApi mgr tkn

getAccounts :: Mondo AccountsResponse
getAccounts = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 0) $ fullApi mgr tkn
    lift $ lift fn

getBalance :: AccountID -> Mondo Balance
getBalance acc = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 1) $ fullApi mgr tkn
    lift $ lift $ fn (Just acc)

getTransaction :: TransactionID -> Bool -> Mondo Transaction
getTransaction tr expandMerchant = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 0) $ transactionsApi mgr tkn
        ex = if expandMerchant then Just "merchant" else Nothing
    lift $ lift (transaction <$> fn tr ex)

listTransactions :: AccountID -> Mondo Transactions
listTransactions acc = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 1) $ transactionsApi mgr tkn
    lift $ lift $ fn (Just acc)

annotateTransaction :: TransactionID -> Metadata -> Mondo Transaction
annotateTransaction tr meta = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 2) $ transactionsApi mgr tkn
    lift $ lift (transaction <$> fn tr meta)

-- | `createFeedItem item' creates a new feed item.
createFeedItem :: FeedItem -> Mondo ()
createFeedItem item = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 0) $ feedApi mgr tkn
    lift $ lift $ toUnit <$> fn item

registerWebhook :: Webhook -> Mondo Webhook
registerWebhook hook = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 0) $ webhooksApi mgr tkn
    lift $ lift $ fn hook

listWebhooks :: AccountID -> Mondo Webhooks
listWebhooks acc = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 1) $ webhooksApi mgr tkn
    lift $ lift $ fn (Just acc)

deleteWebhook :: WebhookID -> Mondo ()
deleteWebhook hookID = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 2) $ webhooksApi mgr tkn
    lift $ lift $ toUnit <$> fn hookID

uploadAttachment :: FileUploadReq -> Mondo FileUploadRes
uploadAttachment req = do
    mgr <- manager
    tkn <- credential
    let
        fn =  getNth (Proxy :: Proxy 5) $ fullApi mgr tkn
    lift $ lift $ fn req

registerAttachment :: Attachment -> Mondo Attachment
registerAttachment att = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 6) $ fullApi mgr tkn
    lift $ lift $ fn att

removeAttachment :: AttachmentID -> Mondo ()
removeAttachment attID = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 7) $ fullApi mgr tkn
    lift $ lift $ toUnit <$> fn attID

--------------------------------------------------------------------------------

class GetNth (n :: Nat) a b | n a -> b where
    getNth :: Proxy n -> a -> b

instance x ~ y => GetNth 0 x y where
    getNth _ x = x

instance {-# OVERLAPPING #-}
  GetNth 0 (x :<|> y) x where
      getNth _ (x :<|> _) = x

instance
  (GetNth (n-1) x y) => GetNth n (a :<|> x) y where
      getNth _ (_ :<|> x) = getNth (Proxy :: Proxy (n-1)) x

class GetLast a b | a -> b where
    getLast :: a -> b

instance {-# OVERLAPPING #-} (GetLast b c) => GetLast (a :<|> b) c where
    getLast (_ :<|> b) = getLast b

instance a ~ b => GetLast a b where
    getLast a = a

--------------------------------------------------------------------------------
