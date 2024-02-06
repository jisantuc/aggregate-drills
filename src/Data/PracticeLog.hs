{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.PracticeLog where

import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Data.Frontmatter (parseYamlFrontmatterEither)
import Data.ByteString (ByteString)

data Drill = TenBallRunOut deriving (Eq, Show)

instance FromJSON Drill where
  parseJSON (Aeson.String "10 Ball Run Out") = pure TenBallRunOut
  parseJSON _ = fail "must be 10 Ball Run out"

data Frontmatter = TenBallRunoutFrontmatter
  { date :: String,
    drill :: Drill,
    handicap :: Maybe Int,
    location :: String,
    tableSize :: Int,
    totalBalls :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)

readHeader :: ByteString -> Either String Frontmatter
readHeader = parseYamlFrontmatterEither
