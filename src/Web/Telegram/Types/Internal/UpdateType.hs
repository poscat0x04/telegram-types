module Web.Telegram.Types.Internal.UpdateType where

import Common

data UpdateType
  = TMessage
  | TEditedMessage
  | TChannelPost
  | TEditedChannelPost
  | TInlineQuery
  | TChosenInlineResult
  | TCallbackQuery
  | TShippingQuery
  | TPreCheckoutQuery
  | TPollUpdate
  | TPollAnswer
  deriving stock (Show, Eq, Enum, Ord)

makePrismLabels ''UpdateType
deriveJSON
  ( sumSnake
      { constructorTagModifier = camelTo2 '_' . tryStrip "T"
      }
  )
  ''UpdateType
