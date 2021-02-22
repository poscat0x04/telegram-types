module Web.Telegram.Types.Internal.Voice where

import Common

-- | A voice note
data Voice = Voice
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | Duration of the audio in seconds as defined by sender
    duration :: Int,
    -- | MIME type of the file as defined by sender
    mimeType :: Maybe Text,
    -- | File size
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''Voice
deriveJSON snake ''Voice
