module Web.Telegram.Types.Internal.API.SendMediaGroup where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputMedia

data SendMediaGroup = SendMediaGroup
  { chatId :: ChatId,
    media :: [InputMedia],
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    allowSendingWithoutReply :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''SendMediaGroup
deriveToJSON snake ''SendMediaGroup
