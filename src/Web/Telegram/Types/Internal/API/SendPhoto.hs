module Web.Telegram.Types.Internal.API.SendPhoto where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.ReplyMarkup

data SendPhoto = SendPhoto
  { chatId :: ChatId,
    photo :: InputFile 'Normal,
    caption :: Maybe Text,
    disableWebPagePreview :: Maybe Bool,
    parseMode :: Maybe ParseMode,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SendPhoto

mkLabel ''SendPhoto
