{-# LANGUAGE RecordWildCards #-}
module Majong.Pai (
  Pai(..)
) where

import Majong.Suit

data Pai = Pai {
  suit :: Suit,
  num :: Int,
  red :: Bool
}
  deriving (Eq, Ord)

instance Show Pai where
  show Pai{..} =
    if suit == Jihai then
      return $ "ESWNPFC" !! (num - 1)
    else
      show num ++ (suitShow suit) ++ (if red then "r" else "")
    where
      suitShow Manzu = "m"
      suitShow Pinzu = "p"
      suitShow Sozu  = "s"
      suitShow Jihai = "z" -- assert false
