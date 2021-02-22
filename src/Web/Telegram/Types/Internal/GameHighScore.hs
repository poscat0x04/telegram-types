module Web.Telegram.Types.Internal.GameHighScore where

import Common
import Web.Telegram.Types.Internal.User

-- | One row of the high scores table for a game
data GameHighScore = GameHighScore
  { -- | Position in high score table for the game
    position :: Int,
    -- | User
    user :: User,
    -- | Score
    score :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''GameHighScore
deriveJSON snake ''GameHighScore
