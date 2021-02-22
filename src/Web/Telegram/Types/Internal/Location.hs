module Web.Telegram.Types.Internal.Location where

import Common

-- | A point on the map.
data Location = Location
  { -- | Longitude as defined by sender
    longitude :: Float,
    -- | Latitude as defined by sender
    latitude :: Float,
    -- | The radius of uncertainty for the location, measured in meters; 0-1500
    horizontalAccuracy :: Maybe Float,
    -- | Time relative to the message sending date, during which the location
    --   can be updated, in seconds. For active live locations only.
    livePeriod :: Maybe Int,
    -- | The direction in which user is moving, in degrees; 1-360. For active live locations only.
    heading :: Maybe Int,
    -- | Maximum distance for proximity alerts about approaching another chat member,
    --   in meters. For sent live locations only.
    proximityAlertRadius :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''Location
deriveJSON snake ''Location
