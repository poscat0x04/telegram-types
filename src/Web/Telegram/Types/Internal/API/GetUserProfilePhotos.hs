module Web.Telegram.Types.Internal.API.GetUserProfilePhotos where

import Common

data GetUserProfilePhotos = GetUserProfilePhotos
  { userId :: Int,
    offset :: Maybe Int,
    limit :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''GetUserProfilePhotos
deriveToJSON snake ''GetUserProfilePhotos
makeMethod ''GetUserProfilePhotos
