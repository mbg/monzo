--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Mondo.API (
    MondoAuth,

    TransactionsAPI,
    FeedAPI,
    WebhooksAPI,
    AttachmentsAPI,
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

    clientWithRoute Proxy req val =
        clientWithRoute (Proxy :: Proxy api) (mondoAuthReq val req)

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

-- | A type representing Mondo's transactions API.
type TransactionsAPI =
      Capture "transaction_id" TransactionID :>
      QueryParam "expand[]" String :>
      MondoAuth :>
      Get '[JSON] TransactionResponse
 :<|> QueryParam "account_id" AccountID :>
      MondoAuth :>
      Paginated Get '[JSON] Transactions
 :<|> Capture "transaction_id" TransactionID :>
      ReqBody '[FormUrlEncoded] Metadata :>
      MondoAuth :>
      Patch '[JSON] TransactionResponse

-- | A type representing Mondo's feed API.
type FeedAPI =
      ReqBody '[FormUrlEncoded] (FeedItem BasicItem) :>
      MondoAuth :>
      Post '[JSON] Empty

-- | A type representing Mondo's webhooks API.
type WebhooksAPI =
      ReqBody '[FormUrlEncoded] Webhook :>
      MondoAuth :>
      Post '[JSON] Webhook
 :<|> QueryParam "account_id" AccountID :>
      MondoAuth :>
      Get '[JSON] Webhooks
 :<|> Capture "webhook_id" WebhookID :>
      MondoAuth :>
      Delete '[JSON] Empty

-- | A type representing Mondo's attachments API.
type AttachmentsAPI =
      "upload" :>
      ReqBody '[FormUrlEncoded] FileUploadReq :>
      MondoAuth :>
      Post '[JSON] FileUploadRes
 :<|> "register" :>
      ReqBody '[FormUrlEncoded] Attachment :>
      MondoAuth :>
      Post '[JSON] Attachment
 :<|> "deregister" :>
      ReqBody '[FormUrlEncoded] AttachmentID :>
      MondoAuth :>
      Post '[JSON] Empty

-- | A type representing the Mondo API.
type MondoAPI =
      "accounts" :> MondoAuth :> Get '[JSON] AccountsResponse
 :<|> "balance" :> QueryParam "account_id" AccountID :> MondoAuth :> Get '[JSON] Balance
 :<|> "transactions" :> TransactionsAPI
 :<|> "feed" :> FeedAPI
 :<|> "webhooks" :> WebhooksAPI
 :<|> "attachment" :> AttachmentsAPI

-- | `mondoAPI` serves as a proxy value for the `MondoAPI` type.
mondoAPI :: Proxy MondoAPI
mondoAPI = Proxy

-- | The URL of the Mondo API.
mondoURL :: BaseUrl
mondoURL = BaseUrl Https "api.getmondo.co.uk" 443 "/"

--------------------------------------------------------------------------------

-- | The type of computations which interact with Mondo's API.
type Mondo = ReaderT String (ReaderT Manager (ReaderT BaseUrl (ExceptT ServantError IO)))

-- | `withMondo token f` runs a computation `f` which interacts with the Mondo
--   API and is authenticated by an OAuth token `token`.
withMondo :: String -> Mondo a -> IO (Either ServantError a)
withMondo = withMondoAt mondoURL

-- | `withMondoAt baseurl token f` runs a computation `f` which interacts with
--   the Mondo API and is authenticated by an OAuth token `token`, assuming
--   that the Mondo API can be found at `baseurl`.
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

-- | `baseurl` retrives the URL at which the API is located.
baseurl :: Mondo BaseUrl
baseurl = lift $ lift ask

--------------------------------------------------------------------------------

accountsGET
    :<|> balanceGET
    :<|> transactionAPI
    :<|> feedAPI
    :<|> webHookAPI
    :<|> attachmentAPI = client mondoAPI

transactionGET
    :<|> transactionsGET
    :<|> transactionPATCH = transactionAPI

feedItemPOST = feedAPI

webHookPOST
    :<|> webHookGET
    :<|> webHookDELETE = webHookAPI

uploadPOST
    :<|> registerAttachmentPOST
    :<|> removeAttachmentPOST = attachmentAPI

-- | `handleAPI f` retrieves the implicit parameters from a `Mondo a`
--   computation and passes them on explicitly to `f`.
handleAPI :: (String -> Manager -> BaseUrl -> ExceptT ServantError IO a) -> Mondo a
handleAPI api = do
    mgr <- manager
    url <- baseurl
    tkn <- credential
    lift $ lift $ lift $ api tkn mgr url

-- | `listAccounts` lists all of the current user's accounts.
listAccounts :: Mondo [Account]
listAccounts = accounts <$> handleAPI accountsGET

-- | `getBalance acc` returns `acc`'s current balance.
getBalance :: Account -> Mondo Balance
getBalance Account{..} = handleAPI $ balanceGET (Just accountID)

-- | `getTransaction trans expandMerchant` retrives a transaction `trans` and
--   optionally expands information about the merchant if `expandMerchant` is
--   `True`.
getTransaction :: Transaction -> Bool -> Mondo Transaction
getTransaction Transaction{..} expandMerchant =
    transaction <$> handleAPI (transactionGET transactionID ex)
    where
        ex = if expandMerchant then Just "merchant" else Nothing

-- | `listTransactions acc opts` lists all transactions for `acc`.
listTransactions :: Account -> PageOptions -> Mondo [Transaction]
listTransactions Account{..} opts = transactions <$> handleAPI (\tkn ->
    transactionsGET (Just accountID) tkn
        (limit opts) (since opts) (Timestamp <$> before opts))

-- | `annotateTransaction trans metadata` annotates `trans` with some metadata.
annotateTransaction :: Transaction -> Metadata -> Mondo Transaction
annotateTransaction Transaction{..} meta = transaction <$>
    handleAPI (transactionPATCH transactionID meta)

-- | `createFeedItem item` creates a new feed item.
createFeedItem :: FeedItem BasicItem -> Mondo ()
createFeedItem item = toUnit <$> handleAPI (feedItemPOST item)

-- | `registerWebhook webhook` registers `webhook`.
registerWebhook :: Webhook -> Mondo Webhook
registerWebhook hook = handleAPI $ webHookPOST hook

-- | `listWebhooks account` lists all webhooks for `account`.
listWebhooks :: Account -> Mondo Webhooks
listWebhooks Account{..} = handleAPI $ webHookGET (Just accountID)

-- `deleteWebhook webhook` removes `webhook`.
deleteWebhook :: WebhookID -> Mondo ()
deleteWebhook hookID = toUnit <$> handleAPI (webHookDELETE hookID)

-- `uploadAttachment req` makes a request to upload an attachment.
uploadAttachment :: FileUploadReq -> Mondo FileUploadRes
uploadAttachment req = handleAPI $ uploadPOST req

-- `registerAttachment attachment` registers `attachment` for its transaction.
registerAttachment :: Attachment -> Mondo Attachment
registerAttachment att = handleAPI $ registerAttachmentPOST att

-- | `removeAttachment attachment` deletes `attachment` from its transaction.
removeAttachment :: AttachmentID -> Mondo ()
removeAttachment attID = toUnit <$> handleAPI (removeAttachmentPOST attID)

--------------------------------------------------------------------------------
