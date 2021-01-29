{-# LANGUAGE DeriveAnyClass #-}

module Web.Telegram.Types.Internal.InputMedia where

import Common

data ParseMode
  = MarkdownV2
  | HTML
  | Markdown
  deriving stock (Show, Eq, Ord, Enum)

data InputMedia
  = InputMediaAnimation
      { media :: Text,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        width :: Maybe Int,
        height :: Maybe Int,
        duration :: Maybe Int
      }
  | InputMediaAudio
      { media :: Text,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        duration :: Maybe Int,
        performer :: Maybe Text,
        title :: Maybe Text
      }
  | InputMediaDocument
      { media :: Text,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode
      }
  | InputMediaPhoto
      { media :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode
      }
  | InputMediaVideo
      { media :: Text,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode
      }
  deriving stock (Show, Eq)

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

makePrismLabels ''ParseMode
mkLabel ''InputMedia
mkLabel ''InputMessageContent
deriveJSON (defaultOptions {sumEncoding = UntaggedValue}) ''ParseMode

-- FIXME: use sumUntaggedValue
deriveJSON sumSnake ''InputMedia
deriveJSON snake ''InputMessageContent
