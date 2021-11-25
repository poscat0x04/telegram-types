module Web.Telegram.Types.Internal.API.GetChat where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype GetChat = GetChat
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''GetChat
deriveToJSON snake ''GetChat
makeMethod ''GetChat
