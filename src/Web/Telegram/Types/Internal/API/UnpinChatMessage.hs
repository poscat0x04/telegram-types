module Web.Telegram.Types.Internal.API.UnpinChatMessage where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data UnpinChatMessage = UnpinChatMessage
  { chatId :: ChatId,
    messageId :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''UnpinChatMessage
deriveToJSON snake ''UnpinChatMessage
