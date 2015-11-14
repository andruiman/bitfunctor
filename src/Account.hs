{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Account where

import Utils

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import Data.Text.Encoding (decodeUtf8)

-- tfdepth > 1 multibranch
--         = 1 singlebranch
--         = 0 not forging
--         < 0 full multibranch (please do not use - exponential growth)
data Account = Account { publicKey :: B.ByteString
                       , tfdepth   :: Int
                       } deriving (Generic)

instance Show Account where show acc = show $ accountId acc
instance Eq Account where a1 == a2  = accountId a1 == accountId a2
instance Ord Account where compare a1 a2 = compare (accountId a1) (accountId a2)

instance ToJSON Account where
  toJSON (Account pk _) = object [ "publicKey" .= decodeUtf8 (B16.encode $ B.toStrict pk) ]

instance FromJSON Account where
  parseJSON (Object _) = undefined  -- TODO
  parseJSON invalid    = typeMismatch "Account" invalid


accountId :: Account -> Int
accountId acc = fromIntegral $ first8bytesAsNumber $ publicKey acc
