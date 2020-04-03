module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.Update
import Web.Telegram.Types.Internal.Utils.Default

main :: IO ()
main = do
  f <- LBS.readFile "test/Message1"
  print $ (eitherDecode f :: Either String Update)
  LBS.putStrLn $ encode (def :: InputMedia)
