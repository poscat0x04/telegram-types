{-# LANGUAGE DeriveAnyClass #-}

module Web.Telegram.Types.Internal.InputMedia where

import Common
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.MessageEntity

data ParseMode
  = MarkdownV2
  | HTML
  | Markdown
  deriving stock (Show, Eq, Ord, Enum)

-- | The content of a media message to be sent
data InputMedia
  = -- | An animation file (GIF or H.264/MPEG-4 AVC video without sound) to be sent
    InputMediaAnimation
      { -- | File to send
        media :: InputFile 'Attach,
        -- | Thumbnail of the file sent; can be ignored if thumbnail generation for the file is
        --   supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size.
        --   A thumbnail's width and height should not exceed 320.
        thumb :: Maybe (InputFile 'Attach),
        -- | Caption of the photo to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the photo caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity],
        -- | Animation width
        width :: Maybe Int,
        -- | Animation height
        height :: Maybe Int,
        -- | Animation duration
        duration :: Maybe Int
      }
  | -- | An audio file to be treated as music to be sent
    InputMediaAudio
      { -- | File to send
        media :: InputFile 'Attach,
        -- | Thumbnail of the file sent; can be ignored if thumbnail generation for the file is
        --   supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size.
        --   A thumbnail's width and height should not exceed 320.
        thumb :: Maybe (InputFile 'Attach),
        -- | Caption of the photo to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the photo caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity],
        -- | Duration of the audio in seconds
        duration :: Maybe Int,
        -- | Performer of the audio
        performer :: Maybe Text,
        -- | Title of the audio
        title :: Maybe Text
      }
  | -- | A general file to be sent
    InputMediaDocument
      { -- | File to send
        media :: InputFile 'Attach,
        -- | Thumbnail of the file sent; can be ignored if thumbnail generation for the file is
        --   supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size.
        --   A thumbnail's width and height should not exceed 320.
        thumb :: Maybe (InputFile 'Attach),
        -- | Caption of the photo to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the photo caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | A photo to be sent
    InputMediaPhoto
      { -- | File to send
        media :: InputFile 'Attach,
        -- | Caption of the photo to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the photo caption.
        --   See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity]
      }
  | -- | A video to be sent
    InputMediaVideo
      { -- | File to send
        media :: InputFile 'Attach,
        -- | Thumbnail of the file sent; can be ignored if thumbnail generation for the file is
        --   supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size.
        --   A thumbnail's width and height should not exceed 320.
        thumb :: Maybe (InputFile 'Attach),
        -- | Caption of the video to be sent, 0-1024 characters after entities parsing
        caption :: Maybe Text,
        -- | Mode for parsing entities in the video caption. See
        --   [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
        parseMode :: Maybe ParseMode,
        -- | List of special entities that appear in the caption,
        --   which can be specified instead of @parseMode@
        captionEntities :: Maybe [MessageEntity]
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (CollectParts)

makePrismLabels ''ParseMode
mkLabel ''InputMedia
deriveJSON (defaultOptions {sumEncoding = UntaggedValue}) ''ParseMode
deriveToJSON
  ( defaultOptions
      { sumEncoding = TaggedObject "type" undefined,
        omitNothingFields = True,
        fieldLabelModifier = camelTo2 '_' . tryStrip "_",
        constructorTagModifier = camelTo2 '_' . tryStrip "InputMedia"
      }
  )
  ''InputMedia
