module Web.Telegram.Types.Internal.Audio where

import Common
import Web.Telegram.Types.Internal.PhotoSize

-- | An audio file to be treated as music by the Telegram clients.
data Audio = Audio
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | Duration of the audio in seconds as defined by sender
    duration :: Int,
    -- | Performer of the audio as defined by sender or by audio tags
    performer :: Maybe Text,
    -- | Title of the audio as defined by sender or by audio tags
    title :: Maybe Text,
    -- | Original filename as defined by sender
    fileName :: Maybe Text,
    -- | MIME type of the file as defined by sender
    mimeType :: Maybe Text,
    -- | File size
    fileSize :: Maybe Int,
    -- | Thumbnail of the album cover to which the music file belongs
    thumb :: Maybe PhotoSize
  }
  deriving stock (Show, Eq)

mkLabel ''Audio
deriveJSON snake ''Audio
