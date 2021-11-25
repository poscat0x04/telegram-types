module Web.Telegram.Types.Internal.API.SendDocument where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.ReplyMarkup

data SendDocument = SendDocument
  { chatId :: ChatId,
    document :: InputFile 'Normal,
    caption :: Maybe Text,
    parseMode :: Maybe ParseMode,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SendDocument

mkLabel ''SendDocument
makeMethod ''SendDocument
