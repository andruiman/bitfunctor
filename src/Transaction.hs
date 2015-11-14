{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Account

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as B

type Timestamp = Int

data PayloadType = Type | Function | Theorem
                  deriving (Eq, Show, Generic)

instance FromJSON PayloadType where
  parseJSON (String "Type")    = return Type
  parseJSON (String "Func")    = return Func
  parseJSON (String "Theorem") = return Theorem
  parseJSON invalid            = typeMismatch "PayloadType" invalid

--instance ToJSON PayloadType where
--  toEncoding = genericToEncoding defaultOptions

data VerifiableTransactionPayload = VerifiableTransactionPayload {
  uses     :: [(PayloadType, String)],
  provides :: (PayloadType, String),
  code     :: String
} deriving (Eq, Show, Generic)

data Transaction = Transaction { sender :: Account
                               , recipient :: Account
                               , amount :: Int
                               , fee :: Int
                               , txTimestamp :: Timestamp
                               , payload :: VerifiableTransactionPayload
                               } deriving (Show, Generic)


instance FromJSON VerifiableTransactionPayload

--instance ToJSON VerifiableTransactionPayload where
--  toEncoding = genericToEncoding defaultOptions


instance Eq Transaction where
  t1 == t2 =    sender t1 == sender t2
             && recipient t1   == recipient t2
             && txTimestamp t1 == txTimestamp t2

instance Ord Transaction where
  compare t1 t2 = compare (txTimestamp t1) (txTimestamp t2)

instance FromJSON Transaction

--instance ToJSON Transaction where
--  toEncoding = genericToEncoding defaultOptions
