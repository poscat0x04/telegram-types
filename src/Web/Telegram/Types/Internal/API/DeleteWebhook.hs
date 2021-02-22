module Web.Telegram.Types.Internal.API.DeleteWebhook where

import Common

newtype DeleteWebhook = DeleteWebhook
  {dropPendingUpdates :: Maybe Bool}
  deriving stock (Show, Eq)

mkLabel ''DeleteWebhook
deriveJSON snake ''DeleteWebhook
