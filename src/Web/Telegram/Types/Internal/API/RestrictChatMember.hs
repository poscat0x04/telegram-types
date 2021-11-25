module Web.Telegram.Types.Internal.API.RestrictChatMember where

import Common
import Data.Time.Clock.POSIX
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ChatPermissions

data RestrictChatMember = RestrictChatMember
  { chatId :: ChatId,
    userId :: Int,
    permissions :: ChatPermissions,
    untilDate :: Maybe POSIXTime
  }
  deriving stock (Show, Eq)

mkLabel ''RestrictChatMember
deriveToJSON snake ''RestrictChatMember
makeMethod ''RestrictChatMember
