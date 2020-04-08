{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Web.Telegram.Types
  ( -- ** User
    User (..),

    -- ** Message
    Message (..),
    MessageMetadata (..),
    MessageContent (..),

    -- ** Chat
    ChatId (..),
    Chat (..),
    ChatPermissions (..),
    ChatPhoto (..),
    ChatMember (..),

    -- ** Media Types

    -- *** Image
    PhotoSize (..),

    -- *** Audio
    Audio (..),

    -- *** Animation
    Animation (..),

    -- *** Document
    Document (..),

    -- *** Video
    Video (..),

    -- *** Voice
    Voice (..),

    -- *** VideoNote
    VideoNote (..),

    -- *** Contact
    Contact (..),

    -- *** Location
    Location (..),

    -- *** Venue
    Venue (..),

    -- *** PollOption
    PollOption (..),

    -- *** Poll
    Poll (..),
    PollType (..),

    -- *** PollAnswer
    PollAnswer (..),

    -- *** Avatar
    UserProfilePhotos (..),

    -- *** File
    File (..),

    -- ** Stickers
    Sticker (..),
    StickerSet (..),
    MaskPosition (..),

    -- *** Misc
    ParseMode (..),

    -- *** Utilities
    coe,
    liftUnion,
    QueryR,
    Default (..),
  )
where

import Data.Aeson
import Data.Coerce
import Data.OpenUnion
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.Common
import Web.Telegram.Types.Internal.Media
import Web.Telegram.Types.Internal.Sticker
import Web.Telegram.Types.Internal.User
import Web.Telegram.Types.Internal.Utils
import Web.Telegram.Types.Internal.InputMedia

data ChatId
  = ChatId Integer
  | ChanId Text
  deriving (Show, Eq, Generic, Default)
  deriving (FromJSON, ToJSON) via UntaggedSum ChatId

instance ToHttpApiData ChatId where
  toQueryParam (ChatId i) = toQueryParam i
  toQueryParam (ChanId t) = t

-- | Alias to coerce
coe :: Coercible a b => a -> b
coe = coerce

-- | Alias to required param
type QueryR = QueryParam' '[Required, Strict]
