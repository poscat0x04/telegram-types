module Web.Telegram.Types.Internal.PollOption where

import Common

-- | Information about one answer option in a poll.
data PollOption = PollOption
  { -- | Option text, 1-100 characters
    text :: Text,
    -- | Number of users that voted for this option
    voterCount :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''PollOption
deriveJSON snake ''PollOption
