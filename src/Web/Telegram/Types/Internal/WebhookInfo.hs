module Web.Telegram.Types.Internal.WebhookInfo where

import Common
import Data.Time.Clock.POSIX
import Web.Telegram.Types.Internal.UpdateType

-- | Contains information about the current status of a webhook.
data WebhookInfo = WebhookInfo
  { -- | Webhook URL, may be empty if webhook is not set up
    url :: Text,
    -- | True, if a custom certificate was provided for webhook certificate checks
    hasCustomCertificate :: Bool,
    -- | Number of updates awaiting delivery
    pendingUpdateCount :: Int,
    -- | Currently used webhook IP address
    ipAddress :: Maybe Text,
    -- | Unix time for the most recent error that happened when trying to deliver an update via webhook
    lastErrorDate :: Maybe POSIXTime,
    -- | Error message in human-readable format for the most recent error that happened when trying to deliver an update via webhook
    lastErrorMessage :: Maybe Text,
    -- | Maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery
    maxConnections :: Maybe Int,
    -- | A list of update types the bot is subscribed to. Defaults to all update types
    allowedUpdates :: Maybe [UpdateType]
  }
  deriving stock (Show, Eq)

mkLabel ''WebhookInfo
deriveJSON snake ''WebhookInfo
