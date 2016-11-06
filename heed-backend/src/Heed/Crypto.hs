module Heed.Crypto where

import Control.Monad.IO.Class
import System.Random.MWC
import Data.Vector.Unboxed
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Char (chr)
import Crypto.Random.Types

import Crypto.KDF.BCrypt (hashPassword)

generateToken :: (MonadIO m) => m Text
generateToken = do
    randoms <- liftIO (withSystemRandom . asGenIO $ \gen -> replicateM 32 (uniformR (65,122) gen))
    liftIO . return . pack $ (chr <$> toList randoms)

generateHash :: (MonadRandom m) => Text -> m Text
generateHash password = decodeUtf8 <$> hashPassword 12 (encodeUtf8 password)
