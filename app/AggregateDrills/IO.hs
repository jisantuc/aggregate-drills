{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module AggregateDrills.IO where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy as BS
import Data.PracticeLog (TenBallRunOutSummary (..), readJSON)
import Data.Text (Text)
import Database.Persist (Entity (Entity), PersistQueryRead (selectFirst), PersistStoreWrite (insert, insertMany_), (==.))
import Database.Persist.Sqlite (runSqlite)
import Database.RunOutDrill
  ( EntityField (..),
    RawRunOutDrillRack,
    associateToSummary,
    fromComponents,
    racksFromFile,
  )
import System.FilePath (dropExtension, (<.>))

parseRunOutDrillPair :: FilePath -> IO (TenBallRunOutSummary, [RawRunOutDrillRack])
parseRunOutDrillPair filePath =
  let csvPath = dropExtension filePath <.> "csv"
   in do
        racks <- racksFromFile csvPath
        metadata <-
          ( BS.readFile
              >=> ( \bs -> case readJSON bs of
                      Right a -> pure a
                      Left s -> fail s
                  )
            )
            filePath
        pure (metadata, racks)

-- look up existing summary for date
-- if it exists, return it and its racks
-- if it doesn't exist, convert this pair to the output pair type by:
-- \* mapping the summary into a RunOutDrill
-- \* inserting it into the database
-- \* using the returned id to convert the RawRunOutDrillRacks into RunOutDrillRacks
loadRunOutDrillPairToDatabase :: Text -> (TenBallRunOutSummary, [RawRunOutDrillRack]) -> IO ()
loadRunOutDrillPairToDatabase databaseName (summary@(TenBallRunoutSummary {date}), racks) = runSqlite databaseName $ do
  selectFirst [RunOutDrillDate ==. date] [] >>= \case
    Just (Entity drillId _) -> liftIO $ print $ "Already found: " ++ show drillId
    Nothing ->
      insert (fromComponents summary (length racks)) >>= \runOutDrillId -> do
        liftIO $ print $ "Inserting racks for " ++ show runOutDrillId
        let runOutDrillRacks = (\rawRack -> associateToSummary rawRack runOutDrillId) <$> racks
        insertMany_ runOutDrillRacks
