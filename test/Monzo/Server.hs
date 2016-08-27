--------------------------------------------------------------------------------
-- Haskell bindings for the Monzo API                                         --
-- Written by Michael B. Gale (michael.gale@cl.cam.ac.uk)                     --
--------------------------------------------------------------------------------

module Monzo.Server where

--------------------------------------------------------------------------------

import Control.Concurrent
import Control.Monad.Trans.Except

import Data.Maybe
import Data.Time.LocalTime
import Data.Time.RFC3339
import Data.Proxy

import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp

import Servant.API
import Servant.Client
import Servant.Server
import Servant.Server.Internal
import Servant.Server.Internal.Router
import Servant.Server.Internal.RoutingApplication

import Monzo
import Monzo.API

--------------------------------------------------------------------------------

instance HasServer api ctx => HasServer (MonzoAuth :> api) ctx where
    type ServerT (MonzoAuth :> api) m = ServerT api m

    route Proxy =
        route (Proxy :: Proxy api)

--------------------------------------------------------------------------------

time :: ZonedTime
time = fromJust $ parseTimeRFC3339 ("2015-11-12T18:37:02Z" :: String)

acc :: Account
acc = Account {
    accountID = "cake_i$_@_1i3",
    accountDescription = "Aperture Science",
    accountCreated = Timestamp time
}

balance :: Balance
balance = Balance {
    balanceValue = 9001,
    balanceCurrency = "GBP",
    balanceSpentToday = 108
}

--------------------------------------------------------------------------------

handler :: Server MonzoAPI
handler = return (AccountsResponse [acc])
     :<|> (\acc -> return balance)
     :<|> undefined

server :: Application
server = serve monzoAPI handler

startWaiApp :: Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
    (port, socket) <- openTestSocket
    let settings = setPort port defaultSettings
    thread <- forkIO $ runSettingsSocket settings socket app
    return (thread, BaseUrl Http "localhost" port "")

endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
    s <- socket AF_INET Stream defaultProtocol
    localhost <- inet_addr "127.0.0.1"
    bind s (SockAddrInet aNY_PORT localhost)
    listen s 1
    port <- socketPort s
    return (fromIntegral port, s)

--------------------------------------------------------------------------------
