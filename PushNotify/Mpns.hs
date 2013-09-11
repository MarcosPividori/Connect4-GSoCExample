-- GSoC 2013 - Communicating with mobile devices.

-- | This library defines an API for communicating with WPhone powered devices, sending Push Notifications through Microsoft Push Notification Service.

module PushNotify.Mpns
    ( -- * MPNS Service
      sendMPNS
      -- * MPNS Settings
    , MPNSAppConfig(..)
    , DeviceURI
      -- * MPNS Messages
    , MPNSType(..)
    , MPNSInterval(..)
    , MPNSmessage(..)
      -- * MPNS Result
    , MPNSresult(..)
    , MPNSinfo(..)
    , MPNSnotifStatus(..)
    , MPNSsubStatus(..)
    , MPNSconStatus(..)
    ) where

import PushNotify.Mpns.Send
import PushNotify.Mpns.Types
