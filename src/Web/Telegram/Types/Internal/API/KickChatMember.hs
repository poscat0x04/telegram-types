module Web.Telegram.Types.Internal.API.KickChatMember where

import Common
import Data.Time.Clock.POSIX
import Web.Telegram.Types.Internal.API.ChatId

data KickChatMember = KickChatMember
  { chatId :: ChatId,
    userId :: Int,
    untilDate :: Maybe POSIXTime
  }
  deriving stock (Show, Eq)

mkLabel ''KickChatMember
deriveToJSON snake ''KickChatMember
