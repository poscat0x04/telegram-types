{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.Update where

import Common
import Web.Telegram.Types.Internal.CallbackQuery
import Web.Telegram.Types.Internal.ChosenInlineResult
import Web.Telegram.Types.Internal.InlineQuery
import Web.Telegram.Types.Internal.Message
import Web.Telegram.Types.Internal.Poll
import Web.Telegram.Types.Internal.PollAnswer
import Web.Telegram.Types.Internal.PreCheckoutQuery
import Web.Telegram.Types.Internal.ShippingQuery

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
  | UpdatePoll Poll
  | UpdatePollAnswer PollAnswer
  deriving stock (Show, Eq)

-- | An incoming update
data Update = Update
  { -- | The update's unique identifier. Update identifiers start from
    --   a certain positive number and increase sequentially. This ID
    --   becomes especially handy if you're using Webhooks, since it
    --   allows you to ignore repeated updates or to restore the correct
    --   update sequence, should they get out of order. If there are no
    --   new updates for at least a week, then identifier of the next
    --   update will be chosen randomly instead of sequentially.
    updateId :: NoFlatten Int,
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

mkLabel ''Update
makePrismLabels ''UpdateContent
makePrismLabels ''UpdateType
deriveJSON sumSnake ''UpdateType
deriveJSON (prefixedSumSnake "Update") ''UpdateContent
