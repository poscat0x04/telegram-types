module Web.Telegram.Types.Internal.API.PinChatMessage where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data PinChatMessage = PinChatMessage
  { chatId :: ChatId,
    messageId :: Int,
    disableNotification :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''PinChatMessage
deriveToJSON snake ''PinChatMessage
makeMethod ''PinChatMessage
