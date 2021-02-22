{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.InlineKeyboardButton where

import Common
import Web.Telegram.Types.Internal.LoginUrl

-- | One button of an inline keyboard.
data InlineKeyboardButton = InlineKeyboardButton
  { -- | Label text on the button
    text :: NoFlatten Text,
    -- | Action attached to the button
    action :: InlineKeyboardButtonAction
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten InlineKeyboardButton instance FromJSON InlineKeyboardButton

deriving via Flatten InlineKeyboardButton instance ToJSON InlineKeyboardButton

data InlineKeyboardButtonAction
  = -- | HTTP or tg:// url to be opened when button is pressed
    ActionUrl Text
  | -- | An HTTP URL used to automatically authorize the user.
    --   Can be used as a replacement for the Telegram Login Widget.
    ActionLoginUrl LoginUrl
  | -- | Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
    ActionCallbackData Text
  | -- | Pressing the button will prompt the user to select one of their chats,
    --   open that chat and insert the bot's username and the specified inline query
    --   in the input field. Can be empty, in which case just the bot's username will be inserted.
    --
    --   __Note:__ This offers an easy way for users to start using your bot in
    --   [inline mode](https://core.telegram.org/bots/inline) when they are currently in a private
    --   chat with it. Especially useful when combined with
    --   [switch_pm…](https://core.telegram.org/bots/api#answerinlinequery) actions –
    --   in this case the user will be automatically returned to the chat they switched
    --   from, skipping the chat selection screen.
    ActionSwitchInlineQuery Text
  | -- | Pressing the button will insert the bot's username and the specified inline
    --   query in the current chat's input field. Can be empty, in which case only the bot's
    --   username will be inserted.
    --
    --   __Note:__ This offers a quick way for the user to open your bot in
    --   [inline mode](https://core.telegram.org/bots/inline) in the same chat
    --   – good for selecting something from multiple options.
    ActionSwitchInlineQueryCurrentChat Text
  | -- | Specify True to send a [Pay button](https://core.telegram.org/bots/api#payments).
    --
    --   __Note:__ This type of button __must__ always be the first button in the first row.
    ActionPay Bool
  deriving stock (Show, Eq)

mkLabel ''InlineKeyboardButton
makePrismLabels ''InlineKeyboardButtonAction
deriveJSON
  ( defaultOptions
      { sumEncoding = ObjectWithSingleField,
        constructorTagModifier = camelTo2 '_' . tryStrip "Action"
      }
  )
  ''InlineKeyboardButtonAction
