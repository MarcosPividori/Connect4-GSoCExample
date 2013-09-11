-- | This Module define the main functions to send Push Notifications.
module PushNotify.General.Send(
    startPushService
  , closePushService
  , sendPush
  ) where

import PushNotify.General.Types
import PushNotify.General.YesodPushApp

import Yesod
import Data.Text                        (Text)
import Data.Monoid
import Data.Default
import Control.Concurrent
import Control.Monad
import Control.Exception as CE
import Network.HTTP.Conduit
import PushNotify.Gcm
import PushNotify.Mpns

-- | 'startPushService' starts the PushService creating a Manager and a Yesod subsite.
--
-- The Manager will be used to send notifications.
--
-- The Yesod subsite will be used to receive registrations and messages from devices as POST HTTP requests.
startPushService :: PushServiceConfig -> IO (PushManager,PushAppSub)
startPushService pConfig = do
                       let cnfg = pushConfig pConfig
                       httpMan <- case (gcmAppConfig cnfg , mpnsAppConfig cnfg) of
                                      (Nothing,Nothing) -> return Nothing
                                      _                 -> do
                                                             m <- newManager def
                                                             return (Just m)
                       return ( PushManager httpMan pConfig
                              , PushAppSub (newMessageCallback pConfig) (newDeviceCallback pConfig))

-- | 'closePushService' stops the Push service.
closePushService :: PushManager -> IO ()
closePushService man = do
                         case httpManager man of
                             Just m -> closeManager m
                             _      -> return ()

forgetConst :: Device -> Text
forgetConst (GCM  x) = x
forgetConst (MPNS x) = x

isGCM  (GCM  _) = True
isGCM  _        = False

isMPNS (MPNS _) = True
isMPNS _        = False

-- | 'sendPush' sends messages to the appropiate Push Servers.
sendPush :: PushManager -> PushNotification -> [Device] -> IO PushResult
sendPush man notif devices = do
                let
                    gcmDevices  = map forgetConst $ filter isGCM  devices
                    mpnsDevices = map forgetConst $ filter isMPNS devices
                    pConfig     = serviceConfig man
                    config      = pushConfig pConfig

                r1 <- case (gcmDevices , gcmAppConfig config , gcmNotif  notif , httpManager man) of
                          (_:_,Just cnf,Just msg,Just m) -> do
                                                                 res <- sendGCM m cnf msg{registration_ids = gcmDevices}
                                                                 return $ toPushResult res
                          _                              -> return def

                r2 <- case (mpnsDevices , mpnsAppConfig config , mpnsNotif notif , httpManager man) of
                          (_:_,Just cnf,Just msg,Just m)    -> do
                                                                 res <- sendMPNS m cnf msg{deviceURIs = mpnsDevices}
                                                                 return $ toPushResult res
                          _                                 -> return def

                let res = r1 <> r2

                when (unRegistered res /= []) $ mapM_ (unRegisteredCallback pConfig) (unRegistered res)

                when (newIds res /= [])       $ mapM_ (newIdCallback        pConfig) (newIds res)

                return res

