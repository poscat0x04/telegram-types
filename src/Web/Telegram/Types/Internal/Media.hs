{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.Types.Internal.Media where

import Data.Aeson
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.User
import Web.Telegram.Types.Internal.Utils

data PhotoSize
  = PhotoSize
      { fileId :: Text,
        fileUniqueId :: Text,
        width :: Integer,
        height :: Integer,
        fileSize :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake PhotoSize
  deriving (ToHttpApiData) via Serialize PhotoSize

data Audio
  = Audio
      { fileId :: Text,
        fileUniqueId :: Text,
        duration :: Integer,
        performer :: Maybe Text,
        title :: Maybe Text,
        mimeType :: Maybe Text,
        fileSize :: Maybe Integer,
        thumb :: Maybe PhotoSize
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Audio
  deriving (ToHttpApiData) via Serialize Audio

data Document
  = Document
      { fileId :: Text,
        fileUniqueId :: Text,
        thumb :: Maybe PhotoSize,
        fileName :: Maybe Text,
        mimeType :: Maybe Text,
        fileSize :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Document
  deriving (ToHttpApiData) via Serialize Document

data Video
  = Video
      { fileId :: Text,
        fileUniqueId :: Text,
        width :: Integer,
        height :: Integer,
        duration :: Integer,
        thumb :: Maybe PhotoSize,
        mimeType :: Maybe Text,
        fileSize :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Video
  deriving (ToHttpApiData) via Serialize Video

data Animation
  = Animation
      { fileId :: Text,
        fileUniqueId :: Text,
        width :: Integer,
        height :: Integer,
        duration :: Integer,
        thumb :: Maybe PhotoSize,
        fileName :: Maybe Text,
        mimeType :: Maybe Text,
        fileSize :: Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Animation
  deriving (ToHttpApiData) via Serialize Animation

data Voice
  = Voice
      { fileId :: Text,
        fileUniqueId :: Text,
        duration :: Integer,
        mimeType :: Maybe Text,
        fileSize :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Voice
  deriving (ToHttpApiData) via Serialize Voice

data VideoNote
  = VideoNote
      { fileId :: Text,
        fileUniqueId :: Text,
        length :: Integer,
        duration :: Integer,
        thumb :: Maybe PhotoSize,
        fileSize :: Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake VideoNote
  deriving (ToHttpApiData) via Serialize VideoNote

data Contact
  = Contact
      { phoneNumber :: Text,
        firstName :: Text,
        lastName :: Maybe Text,
        userId :: Maybe Integer,
        vcard :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Contact
  deriving (ToHttpApiData) via Serialize Contact

data Location
  = Location
      { longitude :: Float,
        latitude :: Float
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via OmitNothing Location
  deriving (ToHttpApiData) via Serialize Location

data Venue
  = Venue
      { location :: Location,
        title :: Text,
        address :: Text,
        foursquareId :: Maybe Text,
        foursquareType :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Venue
  deriving (ToHttpApiData) via Serialize Venue

data PollOption
  = PollOption
      { text :: Text,
        voterCount :: Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake PollOption
  deriving (ToHttpApiData) via Serialize PollOption

data PollAnswer
  = PollAnswer
      { pollId :: Text,
        user :: User,
        optionIds :: [Integer]
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "poll" PollAnswer
  deriving (ToHttpApiData) via Serialize PollAnswer

data PollType
  = Regular
  | Quid
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier CamelToSnake] PollType
  deriving (ToHttpApiData) via Serialize PollType

data Poll
  = Poll
      { pollId :: Text,
        question :: Text,
        options :: [PollOption],
        totalVoterCount :: Integer,
        isClosed :: Bool,
        isAnonymous :: Bool,
        pollType :: PollType,
        allowsMultipleAnswers :: Bool,
        correctOptionId :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "poll" Poll
  deriving (ToHttpApiData) via Serialize Poll

data UserProfilePhotos
  = UserProfilePhotos
      { totalCount :: Integer,
        photos :: [[PhotoSize]]
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake UserProfilePhotos
  deriving (ToHttpApiData) via Serialize UserProfilePhotos

data File
  = File
      { fileId :: Text,
        fileUniqueId :: Text,
        fileSize :: Maybe Integer,
        filePath :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake File
  deriving (ToHttpApiData) via Serialize File
