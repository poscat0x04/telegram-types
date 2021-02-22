{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.KeyboardButton where

import Common
import Web.Telegram.Types.Internal.KeyboardButtonPollType

-- | One button of the reply keyboard
data KeyboardButton = KeyboardButton
  { -- | Text of the button. If no addons are used, it will
    --   be sent as a message when the button is pressed
    text :: NoFlatten Text,
    addon :: Maybe KeyboardButtonAddon
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten KeyboardButton instance FromJSON KeyboardButton

deriving via Flatten KeyboardButton instance ToJSON KeyboardButton

data KeyboardButtonAddon
  = -- | If True, the user's phone number will be sent as a contact when
    --   the button is pressed. Available in private chats only
    RequestContact Bool
  | -- | If True, the user's current location will be sent when the button
    --   is pressed. Available in private chats only
    RequestLocation Bool
  | -- | If specified, the user will be asked to create a poll and send it
    --   to the bot when the button is pressed. Available in private chats only
    RequestPoll KeyboardButtonPollType
  deriving stock (Show, Eq)

mkLabel ''KeyboardButton
makePrismLabels ''KeyboardButtonAddon
deriveJSON
  ( defaultOptions
      { sumEncoding = ObjectWithSingleField,
        constructorTagModifier = camelTo2 '_'
      }
  )
  ''KeyboardButtonAddon
