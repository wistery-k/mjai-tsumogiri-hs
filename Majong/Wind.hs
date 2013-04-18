module Majong.Wind (
  Wind(..)
) where

data Wind = Ton | Nan | Xia | Pei
  deriving (Eq)

instance Show Wind where
  show Ton = "E"
  show Nan = "S"
  show Xia = "W"
  show Pei = "N"