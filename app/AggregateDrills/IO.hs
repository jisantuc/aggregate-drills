module AggregateDrills.IO where

import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BS
import Data.PracticeLog (TenBallRunOutSummary, readJSON)
import Database.RunOutDrill (RawRunOutDrillRack, RunOutDrill, RunOutDrillRack, racksFromFile)
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
loadRunOutDrillPairToDatabase ::
  (TenBallRunOutSummary, [RawRunOutDrillRack]) ->
  IO (RunOutDrill, RunOutDrillRack)
loadRunOutDrillPairToDatabase (summary, racks) = undefined
