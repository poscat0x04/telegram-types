module Web.Telegram.Types.Internal.API.SendInvoice where

import Common
import Web.Telegram.Types.Internal.InlineKeyboardMarkup
import Web.Telegram.Types.Internal.LabeledPrice

data SendInvoice = SendInvoice
  { chatId :: Int,
    title :: Text,
    description :: Text,
    payload :: Text,
    providerToken :: Text,
    startParameter :: Text,
    currency :: Text,
    prices :: [LabeledPrice],
    providerData :: Maybe Text,
    photoUrl :: Maybe Text,
    photoSize :: Maybe Int,
    photoWidth :: Maybe Int,
    photoHeight :: Maybe Int,
    needName :: Maybe Bool,
    needPhoneNumber :: Maybe Bool,
    needEmail :: Maybe Bool,
    needShippingAddress :: Maybe Bool,
    sendPhoneNumberToProvider :: Maybe Bool,
    sendEmailToProvider :: Maybe Bool,
    isFlexible :: Maybe Bool,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    allowSendingWithoutReply :: Maybe Bool,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendInvoice
deriveToJSON snake ''SendInvoice
makeMethod ''SendInvoice
