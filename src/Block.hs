{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Block where

import Account
import Transaction
import Utils

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import Data.Text.Encoding (decodeUtf8)


data Block = Block { transactions :: [Transaction]
                   , baseTarget :: Integer
                   -- theoretically need to move to LocalView
                   , totalDifficulty :: Double
                   , generator :: Account
                   , generationSignature :: B.ByteString
                   , blockTimestamp :: Timestamp
                   } deriving (Show, Generic)

--instance Show Block where show b = show $ blockId b
instance Eq Block where b1 == b2  = blockId b1 == blockId b2
instance Ord Block where compare b1 b2 = compare (blockId b1) (blockId b2)

--instance FromJSON Block

instance ToJSON Block where
  toJSON b@(Block{}) = object [ "transactions" .= transactions b
                            , "baseTarget" .= baseTarget b
                            , "totalDifficulty" .= totalDifficulty b
                            , "generator" .= generator b
                            , "generationSignature" .= decodeUtf8 (B16.encode $ B.toStrict $ generationSignature b)
                            , "blockTimestamp"      .= blockTimestamp b
                            ]
--   toEncoding = genericToEncoding defaultOptions

blockId :: Block -> Int
blockId block = fromIntegral $ first8bytesAsNumber $ generationSignature block
