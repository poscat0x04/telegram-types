{-# LANGUAGE DeriveAnyClass #-}

module Web.Telegram.Types.Internal.InlineQuery where

import Common
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.Keyboard
import Web.Telegram.Types.Internal.Media
import Web.Telegram.Types.Internal.User

data InlineQuery = InlineQuery
  { id :: Text,
    from :: User,
    location :: Maybe Location,
    query :: Text,
    offset :: Text
  }
  deriving stock (Show, Eq)

data InlineQueryResultContent
  = InlineQueryResultContentArticle
      { title :: Text,
        resultInputMessageContent :: InputMessageContent,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        url :: Text,
        hideUrl :: Maybe Bool,
        description :: Maybe Text,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Int,
        thumbHeight :: Maybe Int
      }
  | InlineQueryResultContentPhoto
      { photoUrl :: Text,
        thumbUrl' :: Text,
        photoWidth :: Maybe Int,
        photoHeight :: Maybe Int,
        resultTitle :: Maybe Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentGif
      { gifUrl :: Text,
        gifWidth :: Maybe Int,
        gifHeight :: Maybe Int,
        gifDuration :: Maybe Int,
        thumbUrl' :: Text,
        resultTitle :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentMpeg4Gif
      { mpeg4Url :: Text,
        mpeg4Width :: Maybe Int,
        mpeg4Height :: Maybe Int,
        mpeg4Duration :: Maybe Int,
        thumbUrl' :: Text,
        resultTitle :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentVideo
      { videoUrl :: Text,
        mimeType :: Text,
        thumbUrl' :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        videoWidth :: Maybe Int,
        videoHeight :: Maybe Int,
        videoDuration :: Maybe Int,
        description :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentAudio
      { audioUrl :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        performer :: Maybe Text,
        audioDuration :: Maybe Int,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentVoice
      { voiceUrl :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        voiceDuration :: Maybe Int,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentDocument
      { documentUrl :: Text,
        mimeType :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        description :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Int,
        thumbHeight :: Maybe Int
      }
  | InlineQueryResultContentLocation
      { latitude :: Float,
        longitude :: Float,
        title :: Text,
        horizontalAccuracy :: Maybe Float,
        livePeriod :: Maybe Int,
        heading :: Maybe Int,
        proximityAlertRadius :: Maybe Int,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Int,
        thumbHeight :: Maybe Int
      }
  | InlineQueryResultContentVenue
      { latitude :: Float,
        longitude :: Float,
        title :: Text,
        address :: Text,
        foursquareId :: Text,
        foursquareType :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Int,
        thumbHeight :: Maybe Int
      }
  | InlineQueryResultContentContact
      { phoneNumber :: Text,
        firstName :: Text,
        lastName :: Maybe Text,
        vcard :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Int,
        thumbHeight :: Maybe Int
      }
  | InlineQueryResultContentGame
      { gameShortName :: Text,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  | InlineQueryResultContentCachedPhoto
      { photoFileId :: Text,
        resultTitle :: Maybe Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedGif
      { gifFileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedMpeg4Gif
      { mpeg4FileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedSticker
      { stickerFileId :: Text,
        title :: Text,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedDocument
      { title :: Text,
        documentFileId :: Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedVideo
      { videoFileId :: Text,
        title :: Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedVoice
      { voiceFileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedAudio
      { audioFileId :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup,
        inputMessageContent :: Maybe InputMessageContent
      }
  deriving stock (Show, Eq)

data ChosenInlineResult = ChosenInlineResult
  { resultId :: Text,
    from :: User,
    location :: Maybe Location,
    inlineMessageId :: Maybe Text,
    query :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''InlineQuery
mkLabel ''InlineQueryResultContent
mkLabel ''ChosenInlineResult
deriveJSON snake ''InlineQuery
deriveJSON snake ''InlineQueryResultContent
deriveJSON snake ''ChosenInlineResult
