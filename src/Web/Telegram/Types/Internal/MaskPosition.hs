module Web.Telegram.Types.Internal.MaskPosition where

import Common

data MaskPosition = MaskPosition
  { point :: Text,
    xShift :: Double,
    yShift :: Double,
    scale :: Double
  }
  deriving stock (Show, Eq)

mkLabel ''MaskPosition
deriveJSON snake ''MaskPosition
