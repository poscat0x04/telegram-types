module Web.Telegram.Types.Internal.VideoNote where

import Common
import Web.Telegram.Types.Internal.PhotoSize

-- | A [video message](https://telegram.org/blog/video-messages-and-telescope)
--   (available in Telegram apps as of v4.0)
data VideoNote = VideoNote
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | Video width and height (diameter of the video message) as defined by sender
    length :: Int,
    -- | Duration of the video in seconds as defined by sender
    duration :: Int,
    -- | Video thumbnail
    thumb :: Maybe PhotoSize,
    -- | File size
    fileSize :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''VideoNote
deriveJSON snake ''VideoNote
