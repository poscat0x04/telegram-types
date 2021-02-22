module Web.Telegram.Types.Internal.Game where

import Common
import Web.Telegram.Types.Internal.Animation
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.PhotoSize

-- | A game. Use BotFather to create and edit games,
--   their short names will act as unique identifiers.
data Game = Game
  { -- | Title of the game
    title :: Text,
    -- | Description of the game
    description :: Text,
    -- | Photo that will be displayed in the game message in chats.
    photo :: [PhotoSize],
    -- | Brief description of the game or high scores included in the game message.
    --   Can be automatically edited to include current high scores for the game
    --   when the bot calls setGameScore, or manually edited using editMessageText.
    --   0-4096 characters.
    text :: Maybe Text,
    -- | Special entities that appear in text, such as usernames, URLs, bot commands, etc.
    textEntities :: Maybe MessageEntity,
    -- | Animation that will be displayed in the game message in chats. Upload via BotFather
    animation :: Animation
  }
  deriving stock (Show, Eq)

mkLabel ''Game
deriveJSON snake ''Game
