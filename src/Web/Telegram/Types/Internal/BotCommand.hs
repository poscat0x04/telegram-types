module Web.Telegram.Types.Internal.BotCommand where

import Common

-- | A bot command
data BotCommand = BotCommand
  { -- | Text of the command, 1-32 characters. Can contain only lowercase
    --   English letters, digits and underscores.
    command :: Text,
    -- | Description of the command, 3-256 characters.
    description :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''BotCommand
deriveJSON snake ''BotCommand
