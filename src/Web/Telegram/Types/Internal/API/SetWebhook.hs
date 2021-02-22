module Web.Telegram.Types.Internal.API.SetWebhook where

import Common
import Web.Telegram.Types.Internal.InputFile

data SetWebhook = SetWebhook
  { url :: Text,
    certificate :: Maybe (InputFile 'Normal),
    ipAddress :: Maybe Text,
    maxConnections :: Maybe Int,
    allowedUpdates :: Maybe [Text],
    dropPendingUpdates :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SetWebhook

mkLabel ''SetWebhook
