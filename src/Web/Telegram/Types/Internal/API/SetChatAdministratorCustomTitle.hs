module Web.Telegram.Types.Internal.API.SetChatAdministratorCustomTitle where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data SetChatAdministratorCustomTitle = SetChatAdministratorCustomTitle
  { chatId :: ChatId,
    userId :: Int,
    customTitle :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''SetChatAdministratorCustomTitle
deriveToJSON snake ''SetChatAdministratorCustomTitle
makeMethod ''SetChatAdministratorCustomTitle
