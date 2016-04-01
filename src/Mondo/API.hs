--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Mondo.API (
    MondoAuth,

    TransactionsAPI,
    FeedAPI,
    WebhooksAPI,
    AttachmentsAPI,
    AuthAPI,
    MondoAPI,

    mondoAPI,

    Mondo,

    withMondo,
    withMondoAt,
    
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

-- | Adds pagination options to an API.
type Paginated c cts a =
    QueryParam "limit" Int :>
    QueryParam "since" Since :>
    QueryParam "before" Timestamp :>
    c cts a

--------------------------------------------------------------------------------

type TransactionsAPI =
      Capture "transaction_id" TransactionID :>
      QueryParam "expand[]" String :>
      Get '[JSON] TransactionResponse
 :<|> QueryParam "account_id" AccountID :>
      Paginated Get '[JSON] Transactions
 :<|> Capture "transaction_id" TransactionID :>
      ReqBody '[FormUrlEncoded] Metadata :>
      Patch '[JSON] TransactionResponse

-- | A type representing Mondo's feed API.
type FeedAPI =
      ReqBody '[FormUrlEncoded] (FeedItem BasicItem) :> Post '[JSON] Empty

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

type Mondo = ReaderT String (ReaderT Manager (ReaderT BaseUrl (ExceptT ServantError IO)))

withMondo :: String -> Mondo a -> IO (Either ServantError a)
withMondo = withMondoAt mondoURL

withMondoAt :: BaseUrl -> String -> Mondo a -> IO (Either ServantError a)
withMondoAt url tkn m = do
    manager <- newManager tlsManagerSettings
    runExceptT (runReaderT (runReaderT (runReaderT m tkn) manager) url)

-- | `credential` retrieves the OAuth token used to make API requests.
credential :: Mondo String
credential = ask

-- | `manager` retrives the HTTP client used to make API requests.
manager :: Mondo Manager
manager = lift ask

baseurl :: Mondo BaseUrl
baseurl = lift $ lift ask

--------------------------------------------------------------------------------

fullApi = client mondoAPI

transactionsApi :: Manager -> BaseUrl -> String ->
      (TransactionID -> Maybe String -> ExceptT ServantError IO TransactionResponse)
 :<|> ((Maybe AccountID -> Maybe Int -> Maybe Since -> Maybe Timestamp -> ExceptT ServantError IO Transactions)
 :<|> (TransactionID -> Metadata -> ExceptT ServantError IO TransactionResponse))
transactionsApi mgr url tkn = getNth (Proxy :: Proxy 2) $ fullApi url mgr tkn

feedApi mgr url tkn = getNth (Proxy :: Proxy 3) $ fullApi url mgr tkn
webhooksApi mgr url tkn = getNth (Proxy :: Proxy 4) $ fullApi url mgr tkn

handleAPI :: (Manager -> BaseUrl -> String -> Mondo a) -> Mondo a
handleAPI api = do
    mgr <- manager
    url <- baseurl
    tkn <- credential
    api mgr url tkn

-- | `listAccounts` lists all of the current user's accounts.
listAccounts :: Mondo [Account]
listAccounts = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 0) $ fullApi url mgr tkn
    lift $ lift $ lift $ accounts <$> fn

-- | `getBalance acc` returns `acc`'s current balance.
getBalance :: Account -> Mondo Balance
getBalance Account{..} = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 1) $ fullApi url mgr tkn
    lift $ lift $ lift $ fn (Just accountID)

-- | `getTransaction trans expandMerchant` retrives a transaction `trans` and
--   optionally expands information about the merchant if `expandMerchant` is
--   `True`.
getTransaction :: Transaction -> Bool -> Mondo Transaction
getTransaction Transaction{..} expandMerchant = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 0) $ transactionsApi mgr url tkn
        ex = if expandMerchant then Just "merchant" else Nothing
    lift $ lift $ lift (transaction <$> fn transactionID ex)

-- | `listTransactions acc opts` lists all transactions for `acc`.
listTransactions :: Account -> PageOptions -> Mondo [Transaction]
listTransactions Account{..} opts = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 1) $ transactionsApi mgr url tkn
    lift $ lift $ lift $ transactions <$> fn (Just accountID) (limit opts) (since opts) (Timestamp <$> before opts)

-- | `annotateTransaction trans metadata` annotates `trans` with some metadata.
annotateTransaction :: Transaction -> Metadata -> Mondo Transaction
annotateTransaction Transaction{..} meta = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 2) $ transactionsApi mgr url tkn
    lift $ lift $ lift (transaction <$> fn transactionID meta)

-- | `createFeedItem item' creates a new feed item.
createFeedItem :: FeedItem BasicItem -> Mondo ()
createFeedItem item = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 0) $ feedApi mgr url tkn
    lift $ lift $ lift $ toUnit <$> fn item

registerWebhook :: Webhook -> Mondo Webhook
registerWebhook hook = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 0) $ webhooksApi mgr url tkn
    lift $ lift $ lift $ fn hook

listWebhooks :: Account -> Mondo Webhooks
listWebhooks Account{..} = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 1) $ webhooksApi mgr url tkn
    lift $ lift $ lift $ fn (Just accountID)

deleteWebhook :: WebhookID -> Mondo ()
deleteWebhook hookID = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 2) $ webhooksApi mgr url tkn
    lift $ lift $ lift $ toUnit <$> fn hookID

uploadAttachment :: FileUploadReq -> Mondo FileUploadRes
uploadAttachment req = handleAPI $ \mgr url tkn -> do
    let
        fn =  getNth (Proxy :: Proxy 5) $ fullApi url mgr tkn
    lift $ lift $ lift $ fn req

registerAttachment :: Attachment -> Mondo Attachment
registerAttachment att = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 6) $ fullApi url mgr tkn
    lift $ lift $ lift $ fn att

removeAttachment :: AttachmentID -> Mondo ()
removeAttachment attID = handleAPI $ \mgr url tkn -> do
    let
        fn = getNth (Proxy :: Proxy 7) $ fullApi url mgr tkn
    lift $ lift $ lift $ toUnit <$> fn attID

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
