{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Web.Telegram.Types.Internal.Utils.Stock
  ( Snake,
    OmitNothing,
    UntaggedSum,
    PrefixedSnake,
    Prefixed,
    PrefixedSnake',
  )
where

import Control.Monad
import Data.Char (isLower)
import Data.List (stripPrefix)
import Data.Maybe
  ( fromMaybe,
    listToMaybe,
  )
import Data.Proxy
import Deriving.Aeson
import GHC.TypeLits

type Snake = CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields]

type OmitNothing = CustomJSON '[OmitNothingFields]

type UntaggedSum = CustomJSON '[SumUntaggedValue, OmitNothingFields]

type PrefixedSnake str = CustomJSON '[FieldLabelModifier (StripPrefix str, CamelToSnake), OmitNothingFields]

type Prefixed str = CustomJSON '[FieldLabelModifier (StripPrefix str), OmitNothingFields]

data StrictStrip t

strictStrip :: String -> String -> Maybe String
strictStrip pre s = do
  t <- stripPrefix pre s
  guard $ maybe False (not . isLower) $ listToMaybe t
  return t

instance KnownSymbol k => StringModifier (StrictStrip k) where
  getStringModifier = fromMaybe <*> strictStrip (symbolVal (Proxy @k))

type PrefixedSnake' str = CustomJSON '[FieldLabelModifier (StrictStrip str, CamelToSnake), OmitNothingFields]
