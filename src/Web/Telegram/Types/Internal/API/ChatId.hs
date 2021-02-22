module Web.Telegram.Types.Internal.API.ChatId where

import Common

data ChatId
  = Chat Int
  | Chan Text
  deriving stock (Show, Eq)

makePrismLabels ''ChatId
deriveToJSON sumSnake ''ChatId
