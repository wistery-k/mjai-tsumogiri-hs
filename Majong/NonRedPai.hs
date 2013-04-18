{-# LANGUAGE RecordWildCards #-}

module Majong.NonRedPai (
  NonRedPai(..),
  NonRedTehai,
  isManzu,
  isPinzu,
  isSozu,
  isJihai,
  fromPai
) where

import Data.Map (Map)

import Majong.Suit
import qualified Majong.Pai as P

data NonRedPai = NonRedPai { suit :: Suit, num :: Int }
  deriving (Eq, Ord)

type NonRedTehai = Map NonRedPai Int

isManzu :: NonRedPai -> Bool
isManzu NonRedPai{ suit = Manzu, num = _ } = True
isManzu _ = False

isPinzu :: NonRedPai -> Bool
isPinzu NonRedPai{ suit = Pinzu, num = _ } = True
isPinzu _ = False

isSozu :: NonRedPai -> Bool
isSozu NonRedPai{ suit = Sozu, num = _ } = True
isSozu _ = False

isJihai :: NonRedPai -> Bool
isJihai NonRedPai{ suit = Jihai, num = _ } = True
isJihai _ = False
 
fromPai :: P.Pai -> NonRedPai
fromPai P.Pai{..} = NonRedPai { suit = suit, num = num }
