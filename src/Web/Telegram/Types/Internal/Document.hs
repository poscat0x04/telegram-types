module Web.Telegram.Types.Internal.Document where

import Common
import Web.Telegram.Types.Internal.PhotoSize

-- | A general file (as opposed to photos, voice messages and audio files)
data Document = Document
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | Document thumbnail as defined by sender
    thumb :: Maybe PhotoSize,
    -- | Original filename as defined by sender
    fileName :: Maybe Text,
    -- | MIME type of the file as defined by sender
    mimeType :: Maybe Text,
    -- | File size
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''Document
deriveJSON snake ''Document
