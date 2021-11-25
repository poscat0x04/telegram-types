module Web.Telegram.Types.Internal.API.GetChatMembersCount where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype GetChatMembersCount = GetChatMembersCount
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''GetChatMembersCount
deriveToJSON snake ''GetChatMembersCount
makeMethod ''GetChatMembersCount
