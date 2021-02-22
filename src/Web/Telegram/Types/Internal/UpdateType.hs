module Web.Telegram.Types.Internal.UpdateType where

import Common

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

makePrismLabels ''UpdateType
deriveJSON sumSnake ''UpdateType
