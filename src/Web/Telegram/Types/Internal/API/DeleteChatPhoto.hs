module Web.Telegram.Types.Internal.API.DeleteChatPhoto where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype DeleteChatPhoto = DeleteChatPhoto
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''DeleteChatPhoto
deriveToJSON snake ''DeleteChatPhoto
makeMethod ''DeleteChatPhoto
