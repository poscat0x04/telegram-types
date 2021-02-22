module Web.Telegram.Types.Internal.API.UnpinAllChatMessages where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype UnpinAllChatMessages = UnpinAllChatMessages
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''UnpinAllChatMessages
deriveToJSON snake ''UnpinAllChatMessages
