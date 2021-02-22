module Web.Telegram.Types.Internal.ChatPhoto where

import Common

-- | This object represents a chat photo.
data ChatPhoto = ChatPhoto
  { -- | File identifier of small (160x160) chat photo. This @fileId@ can be used
    --   only for photo download and only for as long as the photo is not changed.
    smallFileId :: Text,
    -- | Unique file identifier of small (160x160) chat photo, which is supposed
    --   to be the same over time and for different bots. Can't be used to download or reuse the file.
    smallFileUniqueId :: Text,
    -- | File identifier of big (640x640) chat photo. This file_id can be used
    --   only for photo download and only for as long as the photo is not changed.
    bigFileId :: Text,
    -- | Unique file identifier of big (640x640) chat photo, which is supposed
    --   to be the same over time and for different bots. Can't be used to download or reuse the file.
    bitFileUniqueId :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''ChatPhoto
deriveJSON snake ''ChatPhoto
