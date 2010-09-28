{-# LANGUAGE GADTs #-}

module Data.NaturalNumber where

import Data.Typeable
import Data.Type.Equality

import TypeLevel.NaturalNumber hiding (NaturalNumber)
import qualified TypeLevel.NaturalNumber as TLN
import TypeLevel.NaturalNumber.Induction

data N a where
    NZero :: N Zero
    NSuccessorTo :: N n -> N (SuccessorTo n)

data UnknownN where
    UnknownN :: NaturalNumber n => N n -> UnknownN

class (TLN.NaturalNumber n, Induction n) => NaturalNumber n where
    asN :: N n
    fromN :: N n -> n
instance NaturalNumber Zero where
    asN = NZero
    fromN _ = undefined
instance NaturalNumber n => NaturalNumber (SuccessorTo n) where
    asN = NSuccessorTo asN
    fromN _ = undefined

instance NaturalNumber n => Typeable (N n) where
    typeOf n = mkTyConApp (mkTyCon $ "N#" ++ (show . naturalNumberAsInt . fromN) n) []

instance Show (N n) where
    show = show . nToInt

instance Show UnknownN where
    show (UnknownN n) = show n

instance Eq (N n) where
    (==) _ _ = True
    (/=) _ _ = False

instance EqT N where
    eqT NZero NZero = Just Refl
    eqT (NSuccessorTo m) (NSuccessorTo n) =
        case m `eqT` n of
             Just Refl -> Just Refl
             Nothing -> Nothing
    eqT _ _ = Nothing

nToInt :: N n -> Int
nToInt NZero = 0
nToInt (NSuccessorTo n) = 1 + nToInt n

unknownNToInt :: UnknownN -> Int
unknownNToInt (UnknownN n) = nToInt n

intToUnknownN :: Int -> UnknownN
intToUnknownN i
  | i < 0 = error $ "Cannot convert negative number " ++ show i ++ " to a natural number."
  | otherwise = go i
  where
    go 0 = UnknownN NZero
    go i = case go (i-1) of UnknownN n -> UnknownN (NSuccessorTo n)

intToN :: NaturalNumber n => Int -> N n
intToN i
  | i == nToInt n = n
  | otherwise = error $ show i ++ " /= " ++ show n
  where n = asN

