module Web.Telegram.Types.Internal.InlineKeyboardMarkup where

import Common
import Web.Telegram.Types.Internal.InlineKeyboardButton

-- | An inline keyboard that appears right next to the message it belongs to.
newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { -- | Array of button rows, each represented by an Array of 'InlineKeyboardButton' objects
    inlineKeyboard :: [[InlineKeyboardButton]]
  }
  deriving stock (Show, Eq)

mkLabel ''InlineKeyboardMarkup
deriveJSON snake ''InlineKeyboardMarkup
