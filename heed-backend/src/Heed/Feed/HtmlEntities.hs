module Heed.Feed.HtmlEntities
  ( decodeHtmlEnt,
  )
where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)

-- | Decode HTMLEntities
decodeHtmlEnt :: T.Text -> T.Text
decodeHtmlEnt = toStrict . toLazyText . htmlEncodedText
