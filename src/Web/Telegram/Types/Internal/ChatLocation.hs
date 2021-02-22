module Web.Telegram.Types.Internal.ChatLocation where

import Common
import Web.Telegram.Types.Internal.Location

-- | A location to which a chat is connected.
data ChatLocation = ChatLocation
  { -- | The location to which the supergroup is connected. Can't be a live location.
    location :: Location,
    -- | Location address; 1-64 characters, as defined by the chat owner
    address :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''ChatLocation
deriveJSON snake ''ChatLocation
