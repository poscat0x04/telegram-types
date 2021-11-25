module Web.Telegram.Types.Internal.API.SetChatPhoto where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputFile

data SetChatPhoto = SetChatPhoto
  { chatId :: ChatId,
    photo :: InputFile 'Normal
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SetChatPhoto

mkLabel ''SetChatPhoto
makeMethod ''SetChatPhoto
