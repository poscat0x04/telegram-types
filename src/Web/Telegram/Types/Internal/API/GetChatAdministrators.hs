module Web.Telegram.Types.Internal.API.GetChatAdministrators where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype GetChatAdministrators = GetChatAdministrators
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''GetChatAdministrators
deriveToJSON snake ''GetChatAdministrators
makeMethod ''GetChatAdministrators
