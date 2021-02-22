module Web.Telegram.Types.Internal.API.EditMessageMedia where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputMedia

data EditMessageMedia = EditMessageMedia
  { chatId :: Maybe ChatId,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text,
    media :: InputMedia
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts EditMessageMedia

mkLabel ''EditMessageMedia
