{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heed.QueryTest
    ( checkAuthTokenTable
    , checkFeedInfoTable
    , checkFeedItemTable
    , checkPrefTable
    , checkSubscriptionTable
    , checkUnreadItemTable
    , checkUserTable
    ) where

import Data.Int (Int64)
import qualified Heed.Database as DB
import qualified Opaleye as O
import qualified Opaleye.Trans as OT

checkAuthTokenTable :: OT.Transaction [Int64]
checkAuthTokenTable = OT.query $ O.countRows (O.queryTable DB.authTokenTable)

checkFeedInfoTable :: OT.Transaction [Int64]
checkFeedInfoTable = OT.query $ O.countRows (O.queryTable DB.feedInfoTable)

checkFeedItemTable :: OT.Transaction [Int64]
checkFeedItemTable = OT.query $ O.countRows (O.queryTable DB.feedItemTable)

checkPrefTable :: OT.Transaction [Int64]
checkPrefTable = OT.query $ O.countRows (O.queryTable DB.userPrefTable)

checkSubscriptionTable :: OT.Transaction [Int64]
checkSubscriptionTable = OT.query $ O.countRows (O.queryTable DB.subscriptionTable)

checkUnreadItemTable :: OT.Transaction [Int64]
checkUnreadItemTable = OT.query $ O.countRows (O.queryTable DB.unreadItemTable)

checkUserTable :: OT.Transaction [Int64]
checkUserTable = OT.query $ O.countRows (O.queryTable DB.userTable)
