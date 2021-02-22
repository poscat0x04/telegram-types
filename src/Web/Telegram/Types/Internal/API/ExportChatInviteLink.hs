module Web.Telegram.Types.Internal.API.ExportChatInviteLink where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype ExportChatInviteLink = ExportChatInviteLink
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''ExportChatInviteLink
deriveToJSON snake ''ExportChatInviteLink
