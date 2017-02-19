module Heed.Crypto where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.KDF.BCrypt (hashPassword)
import Crypto.Random.Types (MonadRandom)
import Data.Char (chr)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector.Unboxed (replicateM, toList)
import System.Random.MWC (asGenIO, uniformR, withSystemRandom)

generateToken
    :: (MonadIO m)
    => m Text
generateToken = do
    randoms <- liftIO (withSystemRandom . asGenIO $ \gen -> replicateM 32 (uniformR (65, 122) gen))
    liftIO . return . pack $ (chr <$> toList randoms)

generateHash
    :: (MonadRandom m)
    => Text -> m Text
generateHash password = decodeUtf8 <$> hashPassword 12 (encodeUtf8 password)
