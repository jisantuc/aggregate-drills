{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.RunOutDrill where

import Database.Persist.Sql
  ( BackendKey (SqlBackendKey),
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )

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

    deriving Eq Show
|]
