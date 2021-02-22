module Web.Telegram.Types.Internal.InputMessageContent where

import Common
import Web.Telegram.Types.Internal.InputMedia

data InputMessageContent
  = InputTextMessageContent
      { messageText :: Text,
        parseMode :: Maybe ParseMode,
        disableWebPagePreview :: Maybe Bool
      }
  | InputLocationMessageContent
      { latitude :: Float,
        longitude :: Float,
        horizontalAccuracy :: Maybe Float,
        livePeriod :: Maybe Int,
        heading :: Maybe Int,
        proximityAlertRadius :: Maybe Int
      }
  | InputVenueMessageContent
      { latitude :: Float,
        longitude :: Float,
        title :: Text,
        address :: Text,
        foursquareId :: Text,
        foursquareType :: Text
      }
  | InputContactMessageContent
      { phoneNumber :: Text,
        firstName :: Text,
        lastName :: Maybe Text,
        vcard :: Maybe Text
      }
  deriving stock (Show, Eq)

mkLabel ''InputMessageContent
deriveJSON
  ( snake
      { sumEncoding = UntaggedValue
      }
  )
  ''InputMessageContent
