module Web.Telegram.Types.Internal.API.GetUpdates where

import Common
import Web.Telegram.Types.Internal.Update

data GetUpdates = GetUpdates
  { offset :: Maybe Int,
    limit :: Maybe Int,
    timeout :: Maybe Int,
    allowedUpdates :: Maybe [UpdateType]
  }
  deriving stock (Show, Eq)

mkLabel ''GetUpdates
deriveJSON snake ''GetUpdates
