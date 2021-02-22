module Web.Telegram.Types.Internal.UserProfilePhotos where

import Common
import Web.Telegram.Types.Internal.PhotoSize (PhotoSize)

-- | A user's profile pictures.
data UserProfilePhotos = UserProfilePhotos
  { -- | Total number of profile pictures the target user has
    totalCount :: Int,
    -- | Requested profile pictures (in up to 4 sizes each)
    photos :: [[PhotoSize]]
  }
  deriving stock (Show, Eq)

mkLabel ''UserProfilePhotos
deriveJSON snake ''UserProfilePhotos
