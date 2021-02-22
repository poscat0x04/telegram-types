module Web.Telegram.Types.Internal.InlineQuery where

import Common
import Web.Telegram.Types.Internal.Location
import Web.Telegram.Types.Internal.User

-- | An incoming inline query. When the user sends an empty query,
--   your bot could return some default or trending results.
data InlineQuery = InlineQuery
  { -- | Unique identifier for this query
    id :: Text,
    -- | Sender
    from :: User,
    -- | Sender location, only for bots that request user location
    location :: Maybe Location,
    -- | Text of the query (up to 256 characters)
    query :: Text,
    -- | Offset of the results to be returned, can be controlled by the bot
    offset :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''InlineQuery
deriveJSON snake ''InlineQuery
