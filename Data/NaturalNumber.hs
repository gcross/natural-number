{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.NaturalNumber where

import Data.Typeable

import TypeLevel.NaturalNumber hiding (NaturalNumber)

data N a where
    NZero :: N Zero
    NSuccessorTo :: N n → N (SuccessorTo n)
  deriving Typeable

class NaturalNumber n where
    asN :: N n
    asInt :: n → Int
instance NaturalNumber Zero where
    asN = NZero
    asInt _ = 0
instance NaturalNumber n => NaturalNumber (SuccessorTo n) where
    asN = NSuccessorTo asN
    asInt n = 1 + asInt (predecessorOf n)
