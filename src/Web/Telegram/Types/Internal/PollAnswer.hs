module Web.Telegram.Types.Internal.PollAnswer where

import Common
import Web.Telegram.Types.Internal.User

-- | An answer of a user in a non-anonymous poll.
data PollAnswer = PollAnswer
  { -- | Unique poll identifier
    id :: Text,
    -- | The user, who changed the answer to the poll
    user :: User,
    -- | 0-based identifiers of answer options, chosen by the user.
    --   May be empty if the user retracted their vote.
    optionIds :: [Int]
  }
  deriving stock (Show, Eq)

mkLabel ''PollAnswer
deriveJSON snake ''PollAnswer
