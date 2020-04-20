{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.Types.Internal.InlineQuery where

import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.Keyboard
import Web.Telegram.Types.Internal.Media
import Web.Telegram.Types.Internal.User
import Web.Telegram.Types.Internal.Utils

data InlineQuery
  = IQ
      { queryId :: Text,
        from :: User,
        location :: Maybe Location,
        query :: Text,
        offset :: Text
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake' "query" InlineQuery
  deriving (ToHttpApiData) via Serialize InlineQuery

data InlineQueryResult
  = InlineQueryResultArticle
      { resultType :: Text,
        resultId :: Text,
        title :: Text,
        resultInputMessageContent :: InputMessageContent,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        url :: Text,
        hideUrl :: Maybe Bool,
        description :: Maybe Text,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Integer,
        thumbHeight :: Maybe Integer
      }
  | InlineQueryResultPhoto
      { resultType :: Text,
        resultId :: Text,
        photoUrl :: Text,
        thumbUrl' :: Text,
        photoWidth :: Maybe Integer,
        photoHeight :: Maybe Integer,
        resultTitle :: Maybe Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultGif
      { resultType :: Text,
        resultId :: Text,
        gifUrl :: Text,
        gifWidth :: Maybe Integer,
        gifHeight :: Maybe Integer,
        gifDuration :: Maybe Integer,
        thumbUrl' :: Text,
        resultTitle :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultMpeg4Gif
      { resultType :: Text,
        resultId :: Text,
        mpeg4Url :: Text,
        mpeg4Width :: Maybe Integer,
        mpeg4Height :: Maybe Integer,
        mpeg4Duration :: Maybe Integer,
        thumbUrl' :: Text,
        resultTitle :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultVideo
      { resultType :: Text,
        resultId :: Text,
        videoUrl :: Text,
        mimeType :: Text,
        thumbUrl' :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        videoWidth :: Maybe Integer,
        videoHeight :: Maybe Integer,
        videoDuration :: Maybe Integer,
        description :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultAudio
      { resultType :: Text,
        resultId :: Text,
        audioUrl :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        performer :: Maybe Text,
        audioDuration :: Maybe Integer,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultVoice
      { resultType :: Text,
        resultId :: Text,
        voiceUrl :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        voiceDuration :: Maybe Integer,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultDocument
      { resultType :: Text,
        resultId :: Text,
        documentUrl :: Text,
        mimeType :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        description :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Integer,
        thumbHeight :: Maybe Integer
      }
  | InlineQueryResultLocation
      { resultType :: Text,
        resultId :: Text,
        latitude :: Float,
        longitude :: Float,
        title :: Text,
        livePeriod :: Maybe Integer,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Integer,
        thumbHeight :: Maybe Integer
      }
  | InlineQueryResultVenue
      { resultType :: Text,
        resultId :: Text,
        latitude :: Float,
        longitude :: Float,
        title :: Text,
        address :: Text,
        foursquareId :: Text,
        foursquareType :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Integer,
        thumbHeight :: Maybe Integer
      }
  | InlineQueryResultContact
      { resultType :: Text,
        resultId :: Text,
        phoneNumber :: Text,
        firstName :: Text,
        lastName :: Maybe Text,
        vcard :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Integer,
        thumbHeight :: Maybe Integer
      }
  | InlineQueryResultGame
      { resultType :: Text,
        resultId :: Text,
        gameShortName :: Text,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  | InlineQueryResultCachedPhoto
      { resultType :: Text,
        resultId :: Text,
        photoFileId :: Text,
        resultTitle :: Maybe Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultCachedGif
      { resultType :: Text,
        resultId :: Text,
        gifFileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultCachedMpeg4Gif
      { resultType :: Text,
        resultId :: Text,
        mpeg4FileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultCachedSticker
      { resultType :: Text,
        resultId :: Text,
        stickerFileId :: Text,
        title :: Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultCachedDocument
      { resultType :: Text,
        resultId :: Text,
        title :: Text,
        documentFileId :: Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultCachedVideo
      { resultType :: Text,
        resultId :: Text,
        videoFileId :: Text,
        title :: Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultCachedVoice
      { resultType :: Text,
        resultId :: Text,
        voiceFileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultCachedAudio
      { resultType :: Text,
        resultId :: Text,
        audioFileId :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "result" InlineQueryResult
  deriving (ToHttpApiData) via Serialize InlineQueryResult

data ChosenInlineResult
  = ChosenIR
      { resultId :: Text,
        from :: User,
        location :: Maybe Location,
        inlineMessageId :: Maybe Text,
        query :: Text
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake ChosenInlineResult
  deriving (ToHttpApiData) via Serialize ChosenInlineResult
