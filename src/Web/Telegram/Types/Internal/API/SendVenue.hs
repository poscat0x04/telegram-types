module Web.Telegram.Types.Internal.API.SendVenue where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ReplyMarkup

data SendVenue = SendVenue
  { chatId :: ChatId,
    latitude :: Float,
    longitude :: Float,
    title :: Text,
    address :: Text,
    foursquareId :: Maybe Text,
    foursquareType :: Maybe Text,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendVenue
deriveToJSON snake ''SendVenue
