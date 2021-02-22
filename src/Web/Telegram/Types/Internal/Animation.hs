module Web.Telegram.Types.Internal.Animation where

import Common
import Web.Telegram.Types.Internal.PhotoSize

-- | An animation file (GIF or H.264/MPEG-4 AVC video without sound).
data Animation = Animation
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
    -- | Animation thumbnail as defined by sender
    thumb :: Maybe PhotoSize,
    -- | Original animation filename as defined by sender
    fileName :: Maybe Text,
    -- | MIME type of the file as defined by sender
    mimeType :: Maybe Text,
    -- | File size
    fileSize :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''Animation
deriveJSON snake ''Animation
