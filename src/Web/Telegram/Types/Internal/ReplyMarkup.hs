module Web.Telegram.Types.Internal.ReplyMarkup where

import Common
import Web.Telegram.Types.Internal.ForceReply
import Web.Telegram.Types.Internal.InlineKeyboardMarkup
import Web.Telegram.Types.Internal.ReplyKeyboardMarkup
import Web.Telegram.Types.Internal.ReplyKeyboardRemove

data ReplyMarkup
  = InlineKeyboard InlineKeyboardMarkup
  | ReplyKeyboard ReplyKeyboardMarkup
  | ReplyRemove ReplyKeyboardRemove
  | Force ForceReply
  deriving stock (Show, Eq)

makePrismLabels ''ReplyMarkup
deriveJSON sumSnake ''ReplyMarkup
