{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
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

main :: IO ()
main = runSqlite "database.db" $ do
  runMigration runOutDrillMigration

  runOutDrillId <- insert $ RunOutDrill "2024-02-01" 90 1 15
  insert_ $ RunOutDrillRack runOutDrillId 1 2 5 "bad miss on an easy cut"

  drill <- get runOutDrillId
  liftIO $ print (drill :: Maybe RunOutDrill)

  allDrillsFeb1 <- selectList [RunOutDrillDate ==. "2024-02-01"] []
  racksForDrill <- selectList [RunOutDrillRackDrillId ==. runOutDrillId] []
  liftIO $ print (entityVal <$> racksForDrill)
  liftIO $ print (runOutDrillHandicap . entityVal <$> allDrillsFeb1)
