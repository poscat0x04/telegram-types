module Web.Telegram.Types.Internal.KeyboardButtonPollType where

import Common
import Web.Telegram.Types.Internal.Poll

-- | Type of a poll, which is allowed to be created and
--   sent when the corresponding button is pressed.
newtype KeyboardButtonPollType = KeyboardButtonPollType
  { -- | If 'Quiz' is passed, the user will be allowed to create only polls in
    --   the quiz mode. If 'Regular' is passed, only regular polls will be allowed.
    --   Otherwise, the user will be allowed to create a poll of any type.
    _type :: Maybe PollType
  }
  deriving stock (Show, Eq)

mkLabel ''KeyboardButtonPollType
deriveJSON snake ''KeyboardButtonPollType
