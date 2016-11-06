{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode, decode)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Heed.Commands
import Reflex.Dom hiding ((.~))
import Data.Default (def)

testString :: String
testString = "Hello"

testText :: T.Text
testText = "Hello"

main :: IO ()
main = mainWidget bodyWidget

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
    el "header" $ elClass "h1" "page-title" homePageLink
    clickEvent <- button "Click me"
    let clickStream = const ([toStrict . encode $ GetFeeds] :: [ByteString]) <$> clickEvent
    ws <- webSocket "ws://localhost:8080/" def { _webSocketConfig_send = clickStream }
    wsReceive <- holdDyn (Just NewItems) (decode . fromStrict <$> _webSocket_recv ws)
    display wsReceive
    return ()

homePageLink :: DomBuilder t m => m ()
homePageLink = text "log"
