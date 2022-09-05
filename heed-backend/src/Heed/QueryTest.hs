{-| Moduled used when testing to check if the sql file matches the haskell definitions
    and if the current heed database also matches.
-}
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

-- | Query 'DB.authTokenTable'
checkAuthTokenTable :: OT.Transaction [Int64]
checkAuthTokenTable = OT.query $ O.countRows (O.selectTable DB.authTokenTable)

-- | Query 'DB.feedInfoTable'
checkFeedInfoTable :: OT.Transaction [Int64]
checkFeedInfoTable = OT.query $ O.countRows (O.selectTable DB.feedInfoTable)

-- | Query 'DB.feedItemTable'
checkFeedItemTable :: OT.Transaction [Int64]
checkFeedItemTable = OT.query $ O.countRows (O.selectTable DB.feedItemTable)

-- | Query 'DB.userPrefTable'
checkPrefTable :: OT.Transaction [Int64]
checkPrefTable = OT.query $ O.countRows (O.selectTable DB.userPrefTable)

-- | Query 'DB.subscriptionTable'
checkSubscriptionTable :: OT.Transaction [Int64]
checkSubscriptionTable = OT.query $ O.countRows (O.selectTable DB.subscriptionTable)

-- | Query 'DB.unreadItemTable'
checkUnreadItemTable :: OT.Transaction [Int64]
checkUnreadItemTable = OT.query $ O.countRows (O.selectTable DB.unreadItemTable)

-- | Query 'DB.userTable'
checkUserTable :: OT.Transaction [Int64]
checkUserTable = OT.query $ O.countRows (O.selectTable DB.userTable)
