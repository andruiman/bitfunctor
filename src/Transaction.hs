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
  parseJSON (String "Type")     = return Type
  parseJSON (String "Function") = return Function
  parseJSON (String "Theorem")  = return Theorem
  parseJSON invalid             = typeMismatch "PayloadType" invalid

instance ToJSON PayloadType where
  toJSON Type     = toJSON ("Type"     :: String)
  toJSON Function = toJSON ("Function" :: String)
  toJSON Theorem  = toJSON ("Theorem"  :: String)
 -- toEncoding = genericToEncoding defaultOptions

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
                               } deriving (Show, Generic, Eq)


instance FromJSON VerifiableTransactionPayload

instance ToJSON VerifiableTransactionPayload where
  toJSON (VerifiableTransactionPayload u p c) = object [ "uses" .= u
                                                       , "provides" .= p
                                                       , "code" .= c
                                                       ]
--  toEncoding = genericToEncoding defaultOptions


--instance Eq Transaction where
--  t1 == t2 =    sender t1 == sender t2
--             && recipient t1   == recipient t2
--             && txTimestamp t1 == txTimestamp t2

--instance Ord Transaction where
--  compare t1 t2 = compare (txTimestamp t1) (txTimestamp t2)

instance FromJSON Transaction

instance ToJSON Transaction where
  toJSON tx@(Transaction{}) = object [ "sender" .= sender tx
                                     , "recipient" .= recipient tx
                                     , "amount" .= amount tx
                                     , "fee" .= fee tx
                                     , "txTimestamp" .= txTimestamp tx
                                     , "payload" .= payload tx
                                     ]
--  toEncoding = genericToEncoding defaultOptions
