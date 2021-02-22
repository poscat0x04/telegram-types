module Web.Telegram.Types.Internal.API.SetChatPermissions where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ChatPermissions

data SetChatPermissions = SetChatPermissions
  { chatId :: ChatId,
    permissions :: ChatPermissions
  }
  deriving stock (Show, Eq)

mkLabel ''SetChatPermissions
deriveToJSON snake ''SetChatPermissions
