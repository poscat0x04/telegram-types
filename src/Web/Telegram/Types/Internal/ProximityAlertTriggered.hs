module Web.Telegram.Types.Internal.ProximityAlertTriggered where

import Common
import Web.Telegram.Types.Internal.User

-- | The content of a service message, sent whenever a user in
--   the chat triggers a proximity alert set by another user.
data ProximityAlertTriggered = ProximityAlertTriggered
  { -- | User that triggered the alert
    traveler :: User,
    -- | User that set the alert
    watcher :: User,
    -- | The distance between the users
    distance :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''ProximityAlertTriggered
deriveJSON snake ''ProximityAlertTriggered
