module Web.Telegram.Types.Internal.API.GetChatMember where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data GetChatMember = GetChatMember
  { chatId :: ChatId,
    userId :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''GetChatMember
deriveToJSON snake ''GetChatMember
makeMethod ''GetChatMember
