module Web.Telegram.Types.Internal.File where

import Common

-- | a file ready to be downloaded. The file can be downloaded via the link
--   @https://api.telegram.org/file/bot<token>/<file_path>@. It is guaranteed
--   that the link will be valid for at least 1 hour. When the link expires,
--   a new one can be requested by calling [getFile](https://core.telegram.org/bots/api#getfile).
--
--   __Note:__ Maximum file size to download is 20 MB
data File = File
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | File size, if known
    fileSize :: Maybe Int,
    -- | File path. Use @https://api.telegram.org/file/bot<token>/<file_path>@ to get the file.
    filePath :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''File
deriveJSON snake ''File
