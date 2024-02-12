{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS
import Data.PracticeLog (readJSON)
import Database.Persist
  ( Entity (entityVal),
    PersistStoreRead (get),
    PersistStoreWrite (insert),
    insert_,
    selectList,
    (==.),
  )
import Database.Persist.Sql
  ( runMigration,
  )
import Database.Persist.Sqlite (runSqlite)
import Database.RunOutDrill
  ( EntityField (RunOutDrillDate, RunOutDrillRackDrillId),
    RawRunOutDrillRack (RawRunOutDrillRack),
    RunOutDrill (RunOutDrill, runOutDrillHandicap),
    RunOutDrillRack (RunOutDrillRack),
    goodBreak,
    racksFromFile,
    racksToFile,
    runOutDrillMigration,
  )
import Options.Applicative (execParser, fullDesc, help, helper, info, long, metavar, progDesc, strOption, (<**>))
import qualified Options.Applicative as CLI
import System.FilePath (dropExtension, joinPath, (<.>))
import System.FilePath.Glob (glob)

newtype CLIOpts = CLIOpts
  { rootDirectory :: FilePath
  }
  deriving (Eq, Show)

cliParser :: CLI.Parser CLIOpts
cliParser =
  CLIOpts
    <$> strOption
      ( long "root-directory"
          <> metavar "ROOTDIR"
          <> help "Directory to search for drill files"
      )

main :: IO ()
main =
  let opts =
        info
          (cliParser <**> helper)
          ( fullDesc <> progDesc "Parse pool drill data from vimwiki diary directory"
          )
   in do
        (CLIOpts {rootDirectory}) <- execParser opts
        drillFiles <- glob $ joinPath [rootDirectory, "**/*drill.json"]
        print drillFiles
        let rawRack = RawRunOutDrillRack 1 4 goodBreak "bad miss, oops" 4
        racksToFile [rawRack] "out.csv"
        drillSummaries <-
          ( BS.readFile
              >=> ( \bs ->
                      case readJSON bs of
                        Right a -> pure a
                        Left s -> fail s
                  )
            )
            `traverse` drillFiles
        let csvFiles = (\fp -> dropExtension fp <.> "csv") <$> drillFiles
        print csvFiles
        print drillSummaries
        racks <- racksFromFile (head csvFiles)
        print racks
        runSqlite "database.db" $ do
          _ <- runMigration runOutDrillMigration
          runOutDrillId <- insert $ RunOutDrill "2024-02-01" 90 1 15
          insert_ $ RunOutDrillRack runOutDrillId 1 2 5 "bad miss on an easy cut"

          drill <- get runOutDrillId
          liftIO $ print (drill :: Maybe RunOutDrill)

          allDrillsFeb1 <- selectList [RunOutDrillDate ==. "2024-02-01"] []
          racksForDrill <- selectList [RunOutDrillRackDrillId ==. runOutDrillId] []
          liftIO $ print (entityVal <$> racksForDrill)
          liftIO $ print (runOutDrillHandicap . entityVal <$> allDrillsFeb1)
