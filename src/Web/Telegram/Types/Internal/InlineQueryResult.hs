{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.InlineQueryResult where

import Common
import Web.Telegram.Types.Internal.InlineKeyboardMarkup
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.InputMessageContent
import Web.Telegram.Types.Internal.MessageEntity

data InlineQueryResultContent
  = -- | A link to an article or web page.
    InlineQueryResultContentArticle
      { -- | Title of the result
        title :: Text,
        -- | Content of the message to be sent
        inputMessageContent' :: InputMessageContent,
        -- | URL of the result
        url :: Text,
        -- | Pass True, if you don't want the URL to be shown in the message
        hideUrl :: Maybe Bool,
        -- | Short description of the result
        description :: Maybe Text,
        -- | Url of the thumbnail for the result
        thumbUrl :: Maybe Text,
        -- | Thumbnail width
        thumbWidth :: Maybe Int,
        -- | Thumbnail height
        thumbHeight :: Maybe Int
      }
  | InlineQueryResultContentPhoto
      { -- | A valid URL of the photo. Photo must be in __jpeg__ format.
        --   Photo size must not exceed 5MB
        photoUrl :: Text,
        -- | URL of the thumbnail for the photo
        thumbUrl' :: Text,
        -- | Width of the photo
        photoWidth :: Maybe Int,
        -- | Height of the photo
        photoHeight :: Maybe Int,
        -- | Title for the result
        title' :: Maybe Text,
        description :: Maybe Text,
        -- | Caption of the photo to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the photo caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity],
        -- | Content of the message to be sent instead of the photo
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentGif
      { -- | A valid URL for the GIF file. File size must not exceed 1MB
        gifUrl :: Text,
        -- | Width of the GIF
        gifWidth :: Maybe Int,
        -- | Height of the GIF
        gifHeight :: Maybe Int,
        -- | Duration of the GIF
        gifDuration :: Maybe Int,
        -- | URL of the static (JPEG or GIF) or animated (MPEG4) thumbnail for the result
        thumbUrl' :: Text,
        -- | MIME type of the thumbnail, must be one of "image\/jpeg", "image\/gif",
        --   or "video\/mp4". Defaults to "image\/jpeg"
        thumbMimeType :: Maybe Text,
        -- | Title for the result
        title' :: Maybe Text,
        -- | Caption of the GIF file to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        -- | Content of the message to be sent instead of the GIF animation
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentMpeg4Gif
      { -- | A valid URL for the MP4 file. File size must not exceed 1MB
        mpeg4Url :: Text,
        -- | Video width
        mpeg4Width :: Maybe Int,
        -- | Video height
        mpeg4Height :: Maybe Int,
        -- | Video duration
        mpeg4Duration :: Maybe Int,
        thumbUrl' :: Text,
        -- | MIME type of the thumbnail, must be one of "image\/jpeg", "image\/gif",
        --   or "video\/mp4". Defaults to "image\/jpeg"
        thumbMimeType :: Maybe Text,
        title' :: Maybe Text,
        -- | Caption of the MPEG-4 file to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        -- | Content of the message to be sent instead of the video animation
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentVideo
      { -- | A valid URL for the embedded video player or video file
        videoUrl :: Text,
        -- | Mime type of the content of video url, "text\/html" or "video\/mp4"
        mimeType :: Text,
        -- | URL of the thumbnail (jpeg only) for the video
        thumbUrl' :: Text,
        title :: Text,
        -- | Caption of the video to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the video caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        -- | Video width
        videoWidth :: Maybe Int,
        -- | Video height
        videoHeight :: Maybe Int,
        -- | Video duration in seconds
        videoDuration :: Maybe Int,
        description :: Maybe Text,
        -- | Content of the message to be sent instead of the video.
        --   This field is __required__ if 'InlineQueryResultVideo' is used
        --   to send an HTML-page as a result (e.g., a YouTube video).
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentAudio
      { -- | A valid URL for the audio file
        audioUrl :: Text,
        title :: Text,
        -- | Caption, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the audio caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        -- | Performer
        performer :: Maybe Text,
        -- | Audio duration in seconds
        audioDuration :: Maybe Int,
        -- | Content of the message to be sent instead of the audio
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentVoice
      { -- | A valid URL for the voice recording
        voiceUrl :: Text,
        -- | Recording title
        title :: Text,
        -- | Caption, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the voice message caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        -- | Recording duration in seconds
        voiceDuration :: Maybe Int,
        -- | Content of the message to be sent instead of the voice recording
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentDocument
      { documentUrl :: Text,
        mimeType :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        description :: Maybe Text,
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
        inputMessageContent :: Maybe InputMessageContent,
        thumbUrl :: Maybe Text,
        thumbWidth :: Maybe Int,
        thumbHeight :: Maybe Int
      }
  | InlineQueryResultContentGame
      { gameShortName :: Text
      }
  | InlineQueryResultContentCachedPhoto
      { photoFileId :: Text,
        title' :: Maybe Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity],
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedGif
      { gifFileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedMpeg4Gif
      { mpeg4FileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity],
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedSticker
      { stickerFileId :: Text,
        title :: Text,
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedDocument
      { title :: Text,
        documentFileId :: Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity],
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedVideo
      { videoFileId :: Text,
        title :: Text,
        description :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedVoice
      { voiceFileId :: Text,
        title :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        inputMessageContent :: Maybe InputMessageContent
      }
  | InlineQueryResultContentCachedAudio
      { audioFileId :: Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        captionEntities :: Maybe [MessageEntity],
        inputMessageContent :: Maybe InputMessageContent
      }
  deriving stock (Show, Eq)

-- | One result of an inline query.
data InlineQueryResult = InlineQueryResult
  { -- | Unique identifier for this result, 1-64 Bytes
    id :: NoFlatten Text,
    -- | [Inline keyboard](https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating)
    --   attached to the message
    replyMarkup :: NoFlatten (Maybe InlineKeyboardMarkup),
    content :: InlineQueryResultContent
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten InlineQueryResult instance FromJSON InlineQueryResult

deriving via Flatten InlineQueryResult instance ToJSON InlineQueryResult

mkLabel ''InlineQueryResultContent
mkLabel ''InlineQueryResult
deriveJSON
  ( defaultOptions
      { omitNothingFields = True,
        sumEncoding = TaggedObject "type" undefined,
        constructorTagModifier = camelTo2 '_' . tryStrip "InlineQueryResultContent" . tryStrip "InlineQueryResultContentCached",
        fieldLabelModifier = camelTo2 '_' . tryStrip "_" . tryStripSuffix "'"
      }
  )
  ''InlineQueryResultContent
