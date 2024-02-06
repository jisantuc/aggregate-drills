{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.PracticeLog (readHeader)
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
    RunOutDrill (RunOutDrill, runOutDrillHandicap),
    RunOutDrillRack (RunOutDrillRack),
    runOutDrillMigration,
  )
import System.FilePath.Glob (glob)

main :: IO ()
main = do
  drillFiles <- glob "/home/james/vimwiki/diary/**/*drill.md"
  frontMatters <-
    ( BS.readFile
        >=> ( \bs ->
                case readHeader bs of
                  Right a -> pure a
                  Left s -> fail s
            )
      )
      `traverse` drillFiles
  print frontMatters
  runSqlite "database.db" $ do
    runMigration runOutDrillMigration

    runOutDrillId <- insert $ RunOutDrill "2024-02-01" 90 1 15
    insert_ $ RunOutDrillRack runOutDrillId 1 2 5 "bad miss on an easy cut"

    drill <- get runOutDrillId
    liftIO $ print (drill :: Maybe RunOutDrill)

    allDrillsFeb1 <- selectList [RunOutDrillDate ==. "2024-02-01"] []
    racksForDrill <- selectList [RunOutDrillRackDrillId ==. runOutDrillId] []
    liftIO $ print (entityVal <$> racksForDrill)
    liftIO $ print (runOutDrillHandicap . entityVal <$> allDrillsFeb1)
