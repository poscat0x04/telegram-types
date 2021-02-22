module Web.Telegram.Types.Internal.Dice where

import Common

-- | An animated emoji that displays a random value.
data Dice = Dice
  { -- | Emoji on which the dice throw animation is based
    emoji :: Text,
    -- | Value of the dice, 1-6 for “🎲” and “🎯” base emoji,
    --   1-5 for “🏀” and “⚽” base emoji, 1-64 for “🎰” base emoji
    value :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''Dice
deriveJSON snake ''Dice
