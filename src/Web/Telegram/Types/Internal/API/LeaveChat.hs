module Web.Telegram.Types.Internal.API.LeaveChat where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype LeaveChat = LeaveChat
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''LeaveChat
deriveToJSON snake ''LeaveChat
