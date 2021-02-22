module Web.Telegram.Types.Internal.Venue where

import Common
import Web.Telegram.Types.Internal.Location

-- | Venue location. Can't be a live location
data Venue = Venue
  { -- | Venue location. Can't be a live location
    location :: Location,
    -- | Name of the venue
    title :: Text,
    -- | Address of the venue
    address :: Text,
    -- | Foursquare identifier of the venue
    foursquareId :: Maybe Text,
    -- | Foursquare type of the venue.
    --   (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
    foursquareType :: Maybe Text,
    -- | Google Places identifier of the venue
    googlePlaceId :: Maybe Text,
    -- | Google Places type of the venue.
    --   (See [supported types](https://developers.google.com/places/web-service/supported_types).)
    googlePlaceType :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''Venue
deriveJSON snake ''Venue
