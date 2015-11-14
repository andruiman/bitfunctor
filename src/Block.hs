module Block where

import Account
import Transaction
import Utils

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics


data Block = Block { transactions :: [Transaction]
                   , baseTarget :: Integer
                   -- theoretically need to move to LocalView
                   , totalDifficulty :: Double
                   , generator :: Account
                   , generationSignature :: B.ByteString
                   , blockTimestamp :: Timestamp
                   } deriving (Show)

--instance Show Block where show b = show $ blockId b
instance Eq Block where b1 == b2  = blockId b1 == blockId b2
instance Ord Block where compare b1 b2 = compare (blockId b1) (blockId b2)

-- instance FromJSON Block
--
-- instance ToJSON Block where
--   toEncoding = genericToEncoding defaultOptions

blockId :: Block -> Int
blockId block = fromIntegral $ first8bytesAsNumber $ generationSignature block
