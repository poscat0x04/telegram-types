module Web.Telegram.Types.Internal.Media where

import Common
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.User

data PhotoSize = PhotoSize
  { fileId :: Text,
    fileUniqueId :: Text,
    width :: Int,
    height :: Int,
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

data ChatPhoto = ChatPhoto
  { smallFileId :: Text,
    smallFileUniqueId :: Text,
    bigFileId :: Text,
    bitFileUniqueId :: Text
  }
  deriving stock (Show, Eq)

data Audio = Audio
  { fileId :: Text,
    fileUniqueId :: Text,
    duration :: Int,
    performer :: Maybe Text,
    title :: Maybe Text,
    fileName :: Maybe Text,
    mimeType :: Maybe Text,
    fileSize :: Maybe Int,
    thumb :: Maybe PhotoSize
  }
  deriving stock (Show, Eq)

data Document = Document
  { fileId :: Text,
    fileUniqueId :: Text,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe Text,
    mimeType :: Maybe Text,
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

data Video = Video
  { fileId :: Text,
    fileUniqueId :: Text,
    width :: Int,
    height :: Int,
    duration :: Int,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe Text,
    mimeType :: Maybe Text,
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

data Animation = Animation
  { fileId :: Text,
    fileUniqueId :: Text,
    width :: Int,
    height :: Int,
    duration :: Int,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe Text,
    mimeType :: Maybe Text,
    fileSize :: Int
  }
  deriving stock (Show, Eq)

data Voice = Voice
  { fileId :: Text,
    fileUniqueId :: Text,
    duration :: Int,
    mimeType :: Maybe Text,
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

data VideoNote = VideoNote
  { fileId :: Text,
    fileUniqueId :: Text,
    length :: Int,
    duration :: Int,
    thumb :: Maybe PhotoSize,
    fileSize :: Int
  }
  deriving stock (Show, Eq)

data Contact = Contact
  { phoneNumber :: Text,
    firstName :: Text,
    lastName :: Maybe Text,
    userId :: Maybe Int,
    vcard :: Maybe Text
  }
  deriving stock (Show, Eq)

data Location = Location
  { longitude :: Float,
    latitude :: Float,
    horizontalAccuracy :: Maybe Float,
    livePeriod :: Maybe Int,
    heading :: Maybe Int,
    proximityAlertRadius :: Maybe Int
  }
  deriving stock (Show, Eq)

data Venue = Venue
  { location :: Location,
    title :: Text,
    address :: Text,
    foursquareId :: Maybe Text,
    foursquareType :: Maybe Text,
    googlePlaceId :: Maybe Text,
    googlePlaceType :: Maybe Text
  }
  deriving stock (Show, Eq)

data ProximityAlertTriggered = ProximityAlertTriggered
  { traveler :: User,
    watcher :: User,
    distance :: Int
  }
  deriving stock (Show, Eq)

data Dice = Dice
  { emoji :: Text,
    value :: Int
  }
  deriving stock (Show, Eq)

data UserProfilePhotos = UserProfilePhotos
  { totalCount :: Int,
    photos :: [[PhotoSize]]
  }
  deriving stock (Show, Eq)

data File = File
  { fileId :: Text,
    fileUniqueId :: Text,
    fileSize :: Maybe Int,
    filePath :: Maybe Text
  }
  deriving stock (Show, Eq)

data Game = Game
  { title :: Text,
    description :: Text,
    photo :: [PhotoSize],
    text :: Maybe Text,
    textEntities :: Maybe MessageEntity,
    animation :: Animation
  }
  deriving stock (Show, Eq)

mkLabel ''PhotoSize
mkLabel ''ChatPhoto
mkLabel ''Audio
mkLabel ''Document
mkLabel ''Video
mkLabel ''Animation
mkLabel ''Voice
mkLabel ''VideoNote
mkLabel ''Contact
mkLabel ''Location
mkLabel ''Venue
mkLabel ''ProximityAlertTriggered
mkLabel ''Dice
mkLabel ''UserProfilePhotos
mkLabel ''File
mkLabel ''Game
deriveJSON snake ''PhotoSize
deriveJSON snake ''ChatPhoto
deriveJSON snake ''Audio
deriveJSON snake ''Document
deriveJSON snake ''Video
deriveJSON snake ''Animation
deriveJSON snake ''Voice
deriveJSON snake ''VideoNote
deriveJSON snake ''Contact
deriveJSON snake ''Location
deriveJSON snake ''Venue
deriveJSON snake ''ProximityAlertTriggered
deriveJSON snake ''Dice
deriveJSON snake ''UserProfilePhotos
deriveJSON snake ''File
deriveJSON snake ''Game
