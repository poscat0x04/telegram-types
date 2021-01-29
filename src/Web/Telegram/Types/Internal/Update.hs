{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.Update where

import Common
import Data.Time.Clock.POSIX
import Web.Telegram.Types.Internal.Common
import Web.Telegram.Types.Internal.InlineQuery
import Web.Telegram.Types.Internal.Payment
import Web.Telegram.Types.Internal.Poll

data UpdateContent
  = UpdateMessage Message
  | UpdateEditedMessage Message
  | UpdateChannelPost Message
  | UpdateEditedChannelPost Message
  | UpdateInlineQuery InlineQuery
  | UpdateChosenInlineResult ChosenInlineResult
  | UpdateCallbackQuery CallbackQuery
  | UpdateShippingQuery ShippingQuery
  | UpdatePreCheckoutQuery PreCheckoutQuery
  | UpdatePollUpdate Poll
  | UpdatePollAnswer PollAnswer
  deriving stock (Show, Eq)

-- | An incoming update
data Update = Update
  { updateId :: NoFlatten Int,
    updateContent :: UpdateContent
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten Update instance FromJSON Update

deriving via Flatten Update instance ToJSON Update

data UpdateType
  = Message
  | EditedMessage
  | ChannelPost
  | EditedChannelPost
  | InlineQuery
  | ChosenInlineResult
  | CallbackQuery
  | ShippingQuery
  | PreCheckoutQuery
  | PollUpdate
  | PollAnswer
  deriving stock (Show, Eq, Enum, Ord)

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

mkLabel ''Update
makePrismLabels ''UpdateContent
makePrismLabels ''UpdateType
mkLabel ''WebhookInfo
deriveJSON snake ''WebhookInfo
deriveJSON sumSnake ''UpdateType
deriveJSON (prefixedSumSnake "Update") ''UpdateContent
