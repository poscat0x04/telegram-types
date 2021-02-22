module Web.Telegram.Types.Internal.API.SendContact where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ReplyMarkup

data SendContact = SendContact
  { chatId :: ChatId,
    phoneNumber :: Text,
    firstName :: Text,
    lastName :: Maybe Text,
    vcard :: Maybe Text,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendContact
deriveToJSON snake ''SendContact
