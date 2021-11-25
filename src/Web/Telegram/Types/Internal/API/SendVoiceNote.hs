module Web.Telegram.Types.Internal.API.SendVoiceNote where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.ReplyMarkup

data SendVoiceNote = SendVoiceNote
  { chatId :: ChatId,
    video_note :: Text,
    duration :: Maybe Int,
    length :: Maybe Int,
    parseMode :: Maybe ParseMode,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendVoiceNote
deriveToJSON snake ''SendVoiceNote
makeMethod ''SendVoiceNote
