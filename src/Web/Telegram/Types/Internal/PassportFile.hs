module Web.Telegram.Types.Internal.PassportFile where

import Common
import Data.Time.Clock.POSIX

-- | A file uploaded to Telegram Passport. Currently all Telegram
--   Passport files are in JPEG format when decrypted and don't exceed 10MB.
data PassportFile = PassportFile
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | File size
    fileSize :: Int,
    -- | Unix time when the file was uploaded
    fileDate :: POSIXTime
  }
  deriving stock (Show, Eq)

mkLabel ''PassportFile
deriveJSON snake ''PassportFile
