module Web.Telegram.Types.Internal.API.EditMessageCaption where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InlineKeyboardMarkup
import Web.Telegram.Types.Internal.InputMedia

data EditMessageCaption = EditMessageCaption
  { chatId :: Maybe ChatId,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text,
    caption :: Maybe Text,
    parseMode :: Maybe ParseMode,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''EditMessageCaption
deriveToJSON snake ''EditMessageCaption
makeMethod ''EditMessageCaption
