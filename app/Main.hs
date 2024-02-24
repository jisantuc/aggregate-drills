{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AggregateDrills.IO (loadRunOutDrillPairToDatabase)
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (traverse_)
import Data.PracticeLog (readJSON)
import Data.Text (Text)
import Database.Persist.Sqlite (runMigration, runSqlite)
import Database.RunOutDrill
  ( racksFromFile,
    runOutDrillMigration,
  )
import Options.Applicative
  ( execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    strOption,
    (<**>),
  )
import qualified Options.Applicative as CLI
import System.FilePath (dropExtension, joinPath, (<.>))
import System.FilePath.Glob (glob)

data CLIOpts = CLIOpts
  { rootDirectory :: FilePath,
    databaseFileName :: Text
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
    <*> strOption (long "database-file" <> metavar "DATABASE_FILE" <> help "Path to sqlite3 database file")

main :: IO ()
main =
  let opts =
        info
          (cliParser <**> helper)
          ( fullDesc <> progDesc "Parse pool drill data from vimwiki diary directory"
          )
      loadPairForDrillFile databaseName drillFile = do
        rawDrillFile <- BS.readFile drillFile
        print $ "Loading run out drill metadata from " <> drillFile
        let csvFile = dropExtension drillFile <.> "csv"
        print $ "Loading run out drill racks from " <> csvFile
        drillSummary <- case readJSON rawDrillFile of
          Right a -> pure a
          Left s -> fail s
        racks <- racksFromFile csvFile
        loadRunOutDrillPairToDatabase databaseName (drillSummary, racks)
   in do
        (CLIOpts {rootDirectory, databaseFileName}) <- execParser opts
        runSqlite databaseFileName $ runMigration runOutDrillMigration
        drillFiles <- glob $ joinPath [rootDirectory, "**/*drill.json"]
        -- someday: make this logging
        print drillFiles
        loadPairForDrillFile databaseFileName `traverse_` drillFiles
