{-# LANGUAGE RecordWildCards #-}
module Majong.Suit (
  Suit(..)  
) where

data Suit = Manzu | Pinzu | Sozu | Jihai deriving (Eq, Ord)

