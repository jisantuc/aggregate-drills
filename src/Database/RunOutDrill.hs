{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.RunOutDrill where

import Control.Monad (MonadPlus (mzero))
import Data.ByteString.Char8 (ByteString, empty, hGetSome)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( DefaultOrdered,
    FromField (..),
    FromNamedRecord,
    FromRecord,
    HasHeader (..),
    ToField (..),
    ToRecord,
  )
import Data.Csv.Incremental (Parser (..), decode, encode, encodeRecord)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import qualified Data.PracticeLog as PL
import Database.Persist.Sql
  ( BackendKey (SqlBackendKey),
    PersistEntity (Key),
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (ReadMode), hIsEOF, withFile)

share
  [mkPersist sqlSettings, mkMigrate "runOutDrillMigration"]
  [persistLowerCase|
RunOutDrill
    date String
    totalBalls Int
    handicap Int
    rounds Int
    deriving Eq Show

RunOutDrillRack
    drillId RunOutDrillId
    rackNumber Int
    ballsOnBreak Int
    ballsAfterBreak Int
    whatWentWrong String
    breakScratch Bool default=false
    deriving Eq Show
|]

fromComponents :: PL.TenBallRunOutSummary -> Int -> RunOutDrill
fromComponents
  ( PL.TenBallRunoutSummary
      { PL.date = summaryDate,
        PL.totalBalls = summaryBalls,
        PL.handicap = summaryHandicap
      }
    ) = RunOutDrill summaryDate summaryBalls summaryHandicap

newtype BreakScratch = BreakScratch {unBreakScratch :: Bool} deriving (Eq, Show, Read)

goodBreak :: BreakScratch
goodBreak = BreakScratch False

badBreak :: BreakScratch
badBreak = BreakScratch True

instance ToField BreakScratch where
  toField (BreakScratch b) = if b then "True" else "False"

instance FromField BreakScratch where
  parseField s
    | s == "True" = pure . BreakScratch $ True
    | s == "False" = pure . BreakScratch $ True
    | otherwise = mzero

data RawRunOutDrillRack = RawRunOutDrillRack
  { rackNumber :: Int,
    ballsOnBreak :: Int,
    breakScratch :: BreakScratch,
    whatWentWrong :: String,
    totalBalls :: Int
  }
  deriving (Eq, Show, Generic)

instance FromRecord RawRunOutDrillRack

instance FromNamedRecord RawRunOutDrillRack

instance DefaultOrdered RawRunOutDrillRack

instance ToRecord RawRunOutDrillRack

associateToSummary :: RawRunOutDrillRack -> Key RunOutDrill -> RunOutDrillRack
associateToSummary (RawRunOutDrillRack {rackNumber, ballsOnBreak, breakScratch, whatWentWrong, totalBalls}) drillId =
  RunOutDrillRack
    { runOutDrillRackDrillId = drillId,
      runOutDrillRackRackNumber = rackNumber,
      runOutDrillRackBallsOnBreak = ballsOnBreak,
      runOutDrillRackBallsAfterBreak = totalBalls - ballsOnBreak,
      runOutDrillRackWhatWentWrong = whatWentWrong,
      runOutDrillRackBreakScratch = unBreakScratch breakScratch
    }

feed :: (ByteString -> Parser RawRunOutDrillRack) -> Handle -> IO (Parser RawRunOutDrillRack)
feed k csvFile = do
  hIsEOF csvFile >>= \case
    True -> return $ k empty
    False -> k <$> hGetSome csvFile 4096

racksFromFile :: FilePath -> IO [RawRunOutDrillRack]
racksFromFile path =
  withFile path ReadMode $ \csvFile -> do
    let loop !_ (Fail _ errMsg) = fail errMsg <* exitFailure
        loop acc (Many rs k) = loop (acc <> rs) =<< feed k csvFile
        loop acc (Done rs) = pure (acc <> rs)
    loop [] (decode HasHeader)
      >>= foldlM
        ( \acc result ->
            case result of
              Right r -> pure $ acc ++ [r]
              Left err -> print ("Err!! " <> err) $> acc
        )
        []

racksToFile :: [RawRunOutDrillRack] -> FilePath -> IO ()
racksToFile racks path =
  BL.writeFile path $
    encode $
      foldMap encodeRecord racks
