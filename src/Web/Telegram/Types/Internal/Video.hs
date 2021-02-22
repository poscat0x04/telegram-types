module Web.Telegram.Types.Internal.Video where

import Common
import Web.Telegram.Types.Internal.PhotoSize

-- | A video file
data Video = Video
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | Video width as defined by sender
    width :: Int,
    -- | Video height as defined by sender
    height :: Int,
    -- | Duration of the video in seconds as defined by sender
    duration :: Int,
    -- | Video thumbnail
    thumb :: Maybe PhotoSize,
    -- | Original filename as defined by sender
    fileName :: Maybe Text,
    -- | Mime type of a file as defined by sender
    mimeType :: Maybe Text,
    -- | File size
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''Video
deriveJSON snake ''Video
