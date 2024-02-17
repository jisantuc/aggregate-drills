{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.PracticeLog where

import Data.Aeson (FromJSON (parseJSON), eitherDecode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

data Drill = TenBallRunOut deriving (Eq, Show)

instance FromJSON Drill where
  parseJSON (Aeson.String "10 Ball Run Out") = pure TenBallRunOut
  parseJSON _ = fail "must be 10 Ball Run out"

data TenBallRunOutSummary = TenBallRunoutSummary
  { date :: String,
    drill :: Drill,
    handicap :: Int,
    location :: String,
    tableSize :: Int,
    totalBalls :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)

readJSON :: ByteString -> Either String TenBallRunOutSummary
readJSON = eitherDecode
