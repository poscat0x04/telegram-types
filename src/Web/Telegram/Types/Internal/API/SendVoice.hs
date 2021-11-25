module Web.Telegram.Types.Internal.API.SendVoice where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.ReplyMarkup

data SendVoice = SendVoice
  { chatId :: ChatId,
    voice :: InputFile 'Normal,
    duration :: Maybe Int,
    caption :: Maybe Text,
    parseMode :: Maybe ParseMode,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SendVoice

mkLabel ''SendVoice
makeMethod ''SendVoice
