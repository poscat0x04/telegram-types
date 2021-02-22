{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.CallbackQuery where

import Common
import Web.Telegram.Types.Internal.Message
import Web.Telegram.Types.Internal.User

-- | An incoming callback query from a callback button in an inline keyboard.
data CallbackQuery = CallbackQuery
  { -- | Unique identifier for this query
    id :: NoFlatten Text,
    -- | Sender
    from :: NoFlatten User,
    -- | Global identifier, uniquely corresponding to the chat to which the
    --   message with the callback button was sent. Useful for high scores in games.
    chatInstance :: NoFlatten Text,
    origin :: CallbackQueryOrigin,
    payload :: CallbackQueryPayload
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten CallbackQuery instance ToJSON CallbackQuery

deriving via Flatten CallbackQuery instance FromJSON CallbackQuery

data CallbackQueryOrigin
  = -- | Message with the callback button that originated the query.
    --   Note that message content and message date will not be available if the message is too old
    OriginMessage Message
  | -- | Identifier of the message sent via the bot in inline mode, that originated the query.
    OriginInlineMessage Text
  deriving stock (Show, Eq)

data CallbackQueryPayload
  = -- | Data associated with the callback button.
    --   Be aware that a bad client can send arbitrary data in this field.
    Data Text
  | -- | Short name of a Game to be returned, serves as the unique identifier for the game
    GameShortName Text
  deriving stock (Show, Eq)

mkLabel ''CallbackQuery
makePrismLabels ''CallbackQueryOrigin
makePrismLabels ''CallbackQueryPayload
deriveJSON
  ( defaultOptions
      { sumEncoding = ObjectWithSingleField,
        constructorTagModifier =
          \case
            "OriginMessage" -> "message"
            "OriginInlineMessage" -> "inline_message_id"
            _ -> undefined
      }
  )
  ''CallbackQueryOrigin
deriveJSON
  ( defaultOptions
      { sumEncoding = ObjectWithSingleField,
        constructorTagModifier = camelTo2 '_'
      }
  )
  ''CallbackQueryPayload
