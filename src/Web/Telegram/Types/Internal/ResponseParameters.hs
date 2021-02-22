module Web.Telegram.Types.Internal.ResponseParameters where

import Common

-- | Information about why a request was unsuccessful.
data ResponseParameters = ResponseParameters
  { -- | The group has been migrated to a supergroup with the specified identifier.
    migrateToChatId :: Maybe Int,
    -- | In case of exceeding flood control, the number of seconds left to wait before
    --   the request can be repeated
    retryAfter :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''ResponseParameters
deriveJSON snake ''ResponseParameters
