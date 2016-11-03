{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Query where

import Control.Arrow (returnA)
import qualified Data.Text as T
import Database.PostgreSQL.Simple as PG
import Heed.Database
import qualified Opaleye as O

runUsersQuery :: PG.Connection -> O.Query (O.Column O.PGText) -> IO [T.Text]
runUsersQuery = O.runQuery

getUsers :: O.Query (O.Column O.PGText)
getUsers =
    proc () ->
  do users <- O.queryTable userTable -< ()
     returnA -< userName users
