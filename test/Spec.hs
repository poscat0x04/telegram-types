{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Web.Telegram.Types
import Web.Telegram.Types.Input
import Web.Telegram.Types.Interaction
import Web.Telegram.Types.Update

main :: IO ()
main = do
  f <- LBS.readFile "test/Message1"
  print $ (eitherDecode f :: Either String Update)
  LBS.putStrLn $ encode (def :: InputMedia)
  putStrLn "==== Testring KeyboardButton ====="
  LBS.putStrLn $ encode $ KeyboardButton "button" (Just $ RequestContact True)
  LBS.putStrLn $ encode $ KeyboardButton "button2" Nothing
