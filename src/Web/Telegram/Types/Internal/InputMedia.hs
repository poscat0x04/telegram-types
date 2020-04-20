{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.Types.Internal.InputMedia where

import Data.Aeson
import Data.Hashable
import Data.Maybe (catMaybes)
import Data.OpenUnion
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Servant.Multipart
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.Utils

data ParseMode
  = MarkdownV2
  | HTML
  | Markdown
  deriving (Show, Eq, Ord, Enum, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via OmitNothing ParseMode
  deriving (ToHttpApiData) via Serialize ParseMode

type VideoOrPhoto = Union '[InputMediaPhoto, InputMediaVideo]

instance ToJSON VideoOrPhoto where
  toJSON =
    (\(photo :: InputMediaPhoto) -> toJSON photo)
      @> (\(video :: InputMediaVideo) -> toJSON video)
      @> typesExhausted

instance ToMultipart Mem VideoOrPhoto where
  toMultipart =
    (\(photo :: InputMediaPhoto) -> toMultipart photo)
      @> (\(video :: InputMediaVideo) -> toMultipart video)
      @> typesExhausted

deriving via Serialize VideoOrPhoto instance ToHttpApiData VideoOrPhoto

type InputMedia =
  Union
    '[ InputMediaAnimation,
       InputMediaDocument,
       InputMediaAudio,
       InputMediaVideo,
       InputMediaPhoto
     ]

instance ToJSON InputMedia where
  toJSON =
    (\(anim :: InputMediaAnimation) -> toJSON anim)
      @> (\(doc :: InputMediaDocument) -> toJSON doc)
      @> (\(audio :: InputMediaAudio) -> toJSON audio)
      @> (\(video :: InputMediaVideo) -> toJSON video)
      @> (\(photo :: InputMediaPhoto) -> toJSON photo)
      @> typesExhausted

instance ToMultipart Mem InputMedia where
  toMultipart =
    (\(anim :: InputMediaAnimation) -> toMultipart anim)
      @> (\(doc :: InputMediaDocument) -> toMultipart doc)
      @> (\(audio :: InputMediaAudio) -> toMultipart audio)
      @> (\(video :: InputMediaVideo) -> toMultipart video)
      @> (\(photo :: InputMediaPhoto) -> toMultipart photo)
      @> typesExhausted

deriving via Serialize InputMedia instance ToHttpApiData InputMedia

data InputMediaAnimation
  = InputMediaAnimation
      { media :: Either Text Media,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        width :: Maybe Integer,
        height :: Maybe Integer,
        duration :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (ToHttpApiData) via Serialize InputMediaAnimation

(.=?) :: ToJSON v => Text -> Maybe v -> Maybe (Text, Value)
k .=? v = (k .=) <$> v

instance ToJSON InputMediaAnimation where
  toJSON InputMediaAnimation {..} =
    let m = case media of
          Left t -> t
          Right (Media m') -> "attach://" <> fileName m'
     in object $
          [ "type" .= ("animation" :: Text),
            "media" .= m
          ]
            <> catMaybes
              [ "thumb" .=? thumb,
                "caption" .=? caption,
                "parse_mode" .=? parseMode,
                "width" .=? width,
                "height" .=? height,
                "duration" .=? duration
              ]

instance ToMultipart Mem InputMediaAnimation where
  toMultipart InputMediaAnimation {..} =
    case media of
      Right m -> toMultipart m
      Left _ -> emptyData

data InputMediaAudio
  = InputMediaAudio
      { media :: Either Text Media,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        duration :: Maybe Integer,
        performer :: Maybe Text,
        title :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (ToHttpApiData) via Serialize InputMediaAudio

instance ToJSON InputMediaAudio where
  toJSON InputMediaAudio {..} =
    let m = case media of
          Left t -> t
          Right (Media m') -> "attach://" <> fileName m'
     in object $
          [ "type" .= ("audio" :: Text),
            "media" .= m
          ]
            <> catMaybes
              [ "thumb" .=? thumb,
                "caption" .=? caption,
                "parse_mode" .=? parseMode,
                "duration" .=? duration,
                "performer" .=? performer,
                "title" .=? title
              ]

instance ToMultipart Mem InputMediaAudio where
  toMultipart InputMediaAudio {..} =
    case media of
      Right m -> toMultipart m
      Left _ -> emptyData

data InputMediaDocument
  = InputMediaDocument
      { media :: Either Text Media,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (ToHttpApiData) via Serialize InputMediaDocument

instance ToJSON InputMediaDocument where
  toJSON InputMediaDocument {..} =
    let m = case media of
          Left t -> t
          Right (Media m') -> "attach://" <> fileName m'
     in object $
          [ "type" .= ("document" :: Text),
            "media" .= m
          ]
            <> catMaybes
              [ "thumb" .=? thumb,
                "caption" .=? caption,
                "parse_mode" .=? parseMode
              ]

instance ToMultipart Mem InputMediaDocument where
  toMultipart InputMediaDocument {..} =
    case media of
      Right m -> toMultipart m
      Left _ -> emptyData

data InputMediaPhoto
  = InputMediaPhoto
      { media :: Either Text Media,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (ToHttpApiData) via Serialize InputMediaPhoto

instance ToJSON InputMediaPhoto where
  toJSON InputMediaPhoto {..} =
    let m = case media of
          Left t -> t
          Right (Media m') -> "attach://" <> fileName m'
     in object $
          [ "type" .= ("photo" :: Text),
            "media" .= m
          ]
            <> catMaybes
              [ "caption" .=? caption,
                "parse_mode" .=? parseMode
              ]

instance ToMultipart Mem InputMediaPhoto where
  toMultipart InputMediaPhoto {..} =
    case media of
      Right m -> toMultipart m
      Left _ -> emptyData

data InputMediaVideo
  = InputMediaVideo
      { media :: Either Text Media,
        thumb :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (ToHttpApiData) via Serialize InputMediaVideo

instance ToJSON InputMediaVideo where
  toJSON InputMediaVideo {..} =
    let m = case media of
          Left t -> t
          Right (Media m') -> "attach://" <> fileName m'
     in object $
          [ "type" .= ("video" :: Text),
            "media" .= m
          ]
            <> catMaybes
              [ "thumb" .=? thumb,
                "caption" .=? caption,
                "parse_mode" .=? parseMode
              ]

instance ToMultipart Mem InputMediaVideo where
  toMultipart InputMediaVideo {..} =
    case media of
      Right m -> toMultipart m
      Left _ -> emptyData

data InputMessageContent
  = InputTextMessageContent
      { messageText :: Text,
        parseMode :: Maybe ParseMode,
        disableWebPagePreview :: Maybe Bool
      }
  | InputLocationMessageContent
      { latitude :: Float,
        longitude :: Float,
        livePeriod :: Maybe Integer
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
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake InputMessageContent
  deriving (ToHttpApiData) via Serialize InputMessageContent

------

emptyData :: MultipartData a
emptyData = MultipartData [] []
