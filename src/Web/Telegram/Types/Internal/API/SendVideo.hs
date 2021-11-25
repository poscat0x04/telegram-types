module Web.Telegram.Types.Internal.API.SendVideo where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.ReplyMarkup

data SendVideo = SendVideo
  { chatId :: ChatId,
    video :: InputFile 'Normal,
    duration :: Maybe Int,
    width :: Maybe Int,
    height :: Maybe Int,
    caption :: Maybe Text,
    supportsStreaming :: Maybe Bool,
    parseMode :: Maybe ParseMode,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SendVideo

mkLabel ''SendVideo
makeMethod ''SendVideo
