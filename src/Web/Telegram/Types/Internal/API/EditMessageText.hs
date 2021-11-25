module Web.Telegram.Types.Internal.API.EditMessageText where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InlineKeyboardMarkup
import Web.Telegram.Types.Internal.InputMedia

data EditMessageText = EditMessageText
  { chatId :: Maybe ChatId,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text,
    text :: Text,
    parseMode :: Maybe ParseMode,
    disalbeWebPagePreview :: Maybe Bool,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''EditMessageText
deriveToJSON snake ''EditMessageText
makeMethod ''EditMessageText
