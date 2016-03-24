--------------------------------------------------------------------------------
-- Haskell bindings for the Mondo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Mondo.API where

--------------------------------------------------------------------------------

import GHC.TypeLits

import qualified Data.ByteString.Internal as BS
import Data.Proxy
import Data.Monoid

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

data MondoAccount

instance HasClient api => HasClient (MondoAccount :> api) where
    type Client (MondoAccount :> api) = AccountID -> Client api

    clientWithRoute Proxy req url mgr val =
        clientWithRoute (Proxy :: Proxy api) (addHeader "account_id" val req) url mgr

--------------------------------------------------------------------------------

-- | A type representing the Mondo API.
type MondoAPI =
      MondoAuth :> "accounts" :> Get '[JSON] AccountsResponse
 :<|> MondoAuth :> "balance" :> QueryParam "account_id" AccountID :> Get '[JSON] Balance
 :<|> MondoAuth :> "transactions" :> Capture "transaction_id" String :> Get '[JSON] TransactionResponse
 :<|> MondoAuth :> "transactions" :> QueryParam "account_id" AccountID :> Get '[JSON] Transactions


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

api = client mondoAPI mondoURL

getAccounts :: Mondo AccountsResponse
getAccounts = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 0) $ api mgr
    lift $ lift $ fn tkn

getBalance :: String -> Mondo Balance
getBalance acc = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 1) $ api mgr
    lift $ lift $ fn tkn (Just acc)

getTransaction :: String -> Mondo Transaction
getTransaction tr = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 2) $ api mgr
    lift $ lift (transaction <$> fn tkn tr)

getTransactions :: AccountID -> Mondo Transactions
getTransactions acc = do
    mgr <- manager
    tkn <- credential
    let
        fn = getNth (Proxy :: Proxy 3) $ api mgr
    lift $ lift $ fn tkn (Just acc)

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


--------------------------------------------------------------------------------
