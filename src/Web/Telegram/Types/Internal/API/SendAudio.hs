module Web.Telegram.Types.Internal.API.SendAudio where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.ReplyMarkup

data SendAudio = SendAudio
  { chatId :: ChatId,
    audio :: InputFile 'Normal,
    caption :: Maybe Text,
    duration :: Maybe Int,
    performer :: Maybe Text,
    title :: Maybe Text,
    parseMode :: Maybe ParseMode,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SendAudio

mkLabel ''SendAudio
