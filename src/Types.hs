{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types
  (OutFile(..),
  Block(..),
  LogValues(..),
  LoadNumber(..),
  ExternalForces(..),
  ExternalForce(..),
  CumForces(..)
  )
where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy (ByteString)


data OutFile = OutFile {
 externalForces :: ExternalForces
,blocks :: [Block]
} deriving (Show, Generic)

instance ToJSON OutFile

data Block = Block {
    loadNumber :: Int
,   total :: Double
,   increment :: Double
,   step :: Int
,   plasticityLog :: LogValues
,   crackLog :: LogValues
,   cumForces :: CumForces
} deriving (Show, Generic)

instance ToJSON Block

type LogValues = [Int]
type LoadNumber = Int
type ExternalForce = Double
type ExternalForces = [(LoadNumber, [ExternalForce])]
-- type CumForces = (Maybe Double, Maybe Double, Maybe Double)

newtype CumForces = CumForces (Maybe Double, Maybe Double, Maybe Double)
  deriving (Show, Generic)

instance ToJSON CumForces where
  toJSON (CumForces (a, b, c)) = object ["CumForce1".= a,
    "CumForce2" .= b, "CumForce3" .= c]
