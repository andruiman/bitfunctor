module Utils where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as BI

first8bytesAsNumber :: B.ByteString -> Integer
first8bytesAsNumber bs =  fromIntegral $ BI.runGet BI.getWord64le $ B.take 8 bs
