module Web.Telegram.Types.Internal.API.UnbanChatMember where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data UnbanChatMember = UnbanChatMember
  { chatId :: ChatId,
    userId :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''UnbanChatMember
deriveToJSON snake ''UnbanChatMember
