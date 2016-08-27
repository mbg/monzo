--------------------------------------------------------------------------------
-- Haskell bindings for the Monzo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Monzo.API (
    MonzoAuth,

    TransactionsAPI,
    FeedAPI,
    WebhooksAPI,
    AttachmentsAPI,
    MonzoAPI,

    monzoAPI,

    Monzo,

    withMonzo,
    withMonzoAt,

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

import Monzo.Types

--------------------------------------------------------------------------------

data MonzoAuth

instance HasClient api => HasClient (MonzoAuth :> api) where
    type Client (MonzoAuth :> api) = String -> Client api

    clientWithRoute Proxy req val =
        clientWithRoute (Proxy :: Proxy api) (monzoAuthReq val req)

monzoAuthReq :: String -> Req -> Req
monzoAuthReq token = addHeader "Authorization" ("Bearer " <> token)

--------------------------------------------------------------------------------

-- | Adds pagination options to an API.
type Paginated c cts a =
    QueryParam "limit" Int :>
    QueryParam "since" Since :>
    QueryParam "before" Timestamp :>
    c cts a

--------------------------------------------------------------------------------

-- | A type representing Monzo's transactions API.
type TransactionsAPI =
      Capture "transaction_id" TransactionID :>
      QueryParam "expand[]" String :>
      MonzoAuth :>
      Get '[JSON] TransactionResponse
 :<|> QueryParam "account_id" AccountID :>
      MonzoAuth :>
      Paginated Get '[JSON] Transactions
 :<|> Capture "transaction_id" TransactionID :>
      ReqBody '[FormUrlEncoded] Metadata :>
      MonzoAuth :>
      Patch '[JSON] TransactionResponse

-- | A type representing Monzo's feed API.
type FeedAPI =
      ReqBody '[FormUrlEncoded] (FeedItem BasicItem) :>
      MonzoAuth :>
      Post '[JSON] Empty

-- | A type representing Monzo's webhooks API.
type WebhooksAPI =
      ReqBody '[FormUrlEncoded] Webhook :>
      MonzoAuth :>
      Post '[JSON] Webhook
 :<|> QueryParam "account_id" AccountID :>
      MonzoAuth :>
      Get '[JSON] Webhooks
 :<|> Capture "webhook_id" WebhookID :>
      MonzoAuth :>
      Delete '[JSON] Empty

-- | A type representing Monzo's attachments API.
type AttachmentsAPI =
      "upload" :>
      ReqBody '[FormUrlEncoded] FileUploadReq :>
      MonzoAuth :>
      Post '[JSON] FileUploadRes
 :<|> "register" :>
      ReqBody '[FormUrlEncoded] Attachment :>
      MonzoAuth :>
      Post '[JSON] Attachment
 :<|> "deregister" :>
      ReqBody '[FormUrlEncoded] AttachmentID :>
      MonzoAuth :>
      Post '[JSON] Empty

-- | A type representing the Monzo API.
type MonzoAPI =
      "accounts" :> MonzoAuth :> Get '[JSON] AccountsResponse
 :<|> "balance" :> QueryParam "account_id" AccountID :> MonzoAuth :> Get '[JSON] Balance
 :<|> "transactions" :> TransactionsAPI
 :<|> "feed" :> FeedAPI
 :<|> "webhooks" :> WebhooksAPI
 :<|> "attachment" :> AttachmentsAPI

-- | `MonzoAPI` serves as a proxy value for the `MonzoAPI` type.
monzoAPI :: Proxy MonzoAPI
monzoAPI = Proxy

-- | The URL of the Monzo API.
monzoURL :: BaseUrl
monzoURL = BaseUrl Https "api.getmondo.co.uk" 443 "/"

--------------------------------------------------------------------------------

-- | The type of computations which interact with Monzo's API.
type Monzo = ReaderT String (ReaderT Manager (ReaderT BaseUrl (ExceptT ServantError IO)))

-- | `withMonzo token f` runs a computation `f` which interacts with the Monzo
--   API and is authenticated by an OAuth token `token`.
withMonzo :: String -> Monzo a -> IO (Either ServantError a)
withMonzo = withMonzoAt monzoURL

-- | `withMonzoAt baseurl token f` runs a computation `f` which interacts with
--   the Monzo API and is authenticated by an OAuth token `token`, assuming
--   that the Monzo API can be found at `baseurl`.
withMonzoAt :: BaseUrl -> String -> Monzo a -> IO (Either ServantError a)
withMonzoAt url tkn m = do
    manager <- newManager tlsManagerSettings
    runExceptT (runReaderT (runReaderT (runReaderT m tkn) manager) url)

-- | `credential` retrieves the OAuth token used to make API requests.
credential :: Monzo String
credential = ask

-- | `manager` retrives the HTTP client used to make API requests.
manager :: Monzo Manager
manager = lift ask

-- | `baseurl` retrives the URL at which the API is located.
baseurl :: Monzo BaseUrl
baseurl = lift $ lift ask

--------------------------------------------------------------------------------

accountsGET
    :<|> balanceGET
    :<|> transactionAPI
    :<|> feedAPI
    :<|> webHookAPI
    :<|> attachmentAPI = client monzoAPI

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

-- | `handleAPI f` retrieves the implicit parameters from a `Monzo a`
--   computation and passes them on explicitly to `f`.
handleAPI :: (String -> Manager -> BaseUrl -> ExceptT ServantError IO a) -> Monzo a
handleAPI api = do
    mgr <- manager
    url <- baseurl
    tkn <- credential
    lift $ lift $ lift $ api tkn mgr url

-- | `listAccounts` lists all of the current user's accounts.
listAccounts :: Monzo [Account]
listAccounts = accounts <$> handleAPI accountsGET

-- | `getBalance acc` returns `acc`'s current balance.
getBalance :: Account -> Monzo Balance
getBalance Account{..} = handleAPI $ balanceGET (Just accountID)

-- | `getTransaction trans expandMerchant` retrives a transaction `trans` and
--   optionally expands information about the merchant if `expandMerchant` is
--   `True`.
getTransaction :: Transaction -> Bool -> Monzo Transaction
getTransaction Transaction{..} expandMerchant =
    transaction <$> handleAPI (transactionGET transactionID ex)
    where
        ex = if expandMerchant then Just "merchant" else Nothing

-- | `listTransactions acc opts` lists all transactions for `acc`.
listTransactions :: Account -> PageOptions -> Monzo [Transaction]
listTransactions Account{..} opts = transactions <$> handleAPI (\tkn ->
    transactionsGET (Just accountID) tkn
        (limit opts) (since opts) (Timestamp <$> before opts))

-- | `annotateTransaction trans metadata` annotates `trans` with some metadata.
annotateTransaction :: Transaction -> Metadata -> Monzo Transaction
annotateTransaction Transaction{..} meta = transaction <$>
    handleAPI (transactionPATCH transactionID meta)

-- | `createFeedItem item` creates a new feed item.
createFeedItem :: FeedItem BasicItem -> Monzo ()
createFeedItem item = toUnit <$> handleAPI (feedItemPOST item)

-- | `registerWebhook webhook` registers `webhook`.
registerWebhook :: Webhook -> Monzo Webhook
registerWebhook hook = handleAPI $ webHookPOST hook

-- | `listWebhooks account` lists all webhooks for `account`.
listWebhooks :: Account -> Monzo Webhooks
listWebhooks Account{..} = handleAPI $ webHookGET (Just accountID)

-- `deleteWebhook webhook` removes `webhook`.
deleteWebhook :: WebhookID -> Monzo ()
deleteWebhook hookID = toUnit <$> handleAPI (webHookDELETE hookID)

-- `uploadAttachment req` makes a request to upload an attachment.
uploadAttachment :: FileUploadReq -> Monzo FileUploadRes
uploadAttachment req = handleAPI $ uploadPOST req

-- `registerAttachment attachment` registers `attachment` for its transaction.
registerAttachment :: Attachment -> Monzo Attachment
registerAttachment att = handleAPI $ registerAttachmentPOST att

-- | `removeAttachment attachment` deletes `attachment` from its transaction.
removeAttachment :: AttachmentID -> Monzo ()
removeAttachment attID = toUnit <$> handleAPI (removeAttachmentPOST attID)

--------------------------------------------------------------------------------
