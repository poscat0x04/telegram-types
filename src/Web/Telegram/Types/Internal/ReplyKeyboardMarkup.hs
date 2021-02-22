module Web.Telegram.Types.Internal.ReplyKeyboardMarkup where

import Common
import Web.Telegram.Types.Internal.KeyboardButton

-- | A custom keyboard with reply options
--   (see [Introduction](https://core.telegram.org/bots#keyboards) to bots for details and examples).
data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { -- | Array of button rows, each represented by an Array of KeyboardButton objects
    keyboard :: [[KeyboardButton]],
    -- | Requests clients to resize the keyboard vertically for optimal fit
    --   (e.g., make the keyboard smaller if there are just two rows of buttons).
    --   Defaults to false, in which case the custom keyboard is always of the same
    --    height as the app's standard keyboard.
    resizeKeyboard :: Maybe Bool,
    -- | Requests clients to hide the keyboard as soon as it's been used.
    --   The keyboard will still be available, but clients will automatically
    --   display the usual letter-keyboard in the chat – the user can press a
    --   special button in the input field to see the custom keyboard again. Defaults to false.
    oneTimeKeyboard :: Maybe Bool,
    -- | Use this parameter if you want to show the keyboard to specific users only.
    --   Targets: 1) users that are \@mentioned in the /text/ of the 'Message' object;
    --   2) if the bot's message is a reply (has @replyToMessageId@), sender of the original message.
    --
    --   /Example:/ A user requests to change the bot's language, bot replies to the
    --   request with a keyboard to select the new language. Other users in the group don't see the keyboard.
    selective :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''ReplyKeyboardMarkup
deriveJSON snake ''ReplyKeyboardMarkup
