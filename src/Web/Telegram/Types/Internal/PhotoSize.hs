module Web.Telegram.Types.Internal.PhotoSize where

import Common

-- | One size of a photo or a
--  [file](https://core.telegram.org/bots/api#document)\/[sticker](https://core.telegram.org/bots/api#sticker) thumbnail.
data PhotoSize = PhotoSize
  { -- | Identifier for this file, which can be used to download or reuse the file
    fileId :: Text,
    -- | Unique identifier for this file, which is supposed to be the same over time
    --   and for different bots. Can't be used to download or reuse the file.
    fileUniqueId :: Text,
    -- | Photo width
    width :: Int,
    -- | Photo height
    height :: Int,
    -- | File size
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''PhotoSize
deriveJSON snake ''PhotoSize
