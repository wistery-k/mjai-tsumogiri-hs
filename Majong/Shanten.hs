{-# LANGUAGE RecordWildCards #-}

module Majong.Shanten (
  normal,
  chitoi, 
  kokushi
) where

import qualified Data.Map as M

import Majong.NonRedPai
import qualified Majong.ShantenTable as Table

(&&&) :: (a -> Bool) -> (b -> Bool) -> a -> b -> Bool
(&&&) f g x y = f x && g y

pickMenta :: Int -> NonRedTehai -> Int
pickMenta needMentsu tehai =
  let
    mJ = M.size $ M.filterWithKey (isJihai &&& (>= 3)) tehai
    tJ = M.size $ M.filterWithKey (isJihai &&& (>= 2)) tehai
  in
  let
    mts = [ (mM + mP + mS + mJ, tM + tP + tS + tJ) | 
            (mM, tM) <- Table.get $ hash (M.filterWithKey (\k _ -> isManzu k) tehai),
            (mP, tP) <- Table.get $ hash (M.filterWithKey (\k _ -> isPinzu k) tehai),
            (mS, tS) <- Table.get $ hash (M.filterWithKey (\k _ -> isSozu  k) tehai)
          ]
  in
    minAll $ map (\(m, t) -> 8 - 2 * m - (min t (needMentsu - m))) mts
  where
    hash = sumAll . M.mapWithKey (\k a -> a * 5 ^ (num k - 1))
    sumAll = M.fold (+) 0
    minAll = foldl min 0

normal :: Int -> NonRedTehai -> Int
normal needMentsu tehai =
  minAll (pickMenta needMentsu tehai) (M.mapMaybeWithKey (\k a -> if a >= 2 then Just . pickMenta needMentsu $ remove k 2 tehai else Nothing) tehai)
  where
    minAll = M.fold min
    remove k n mp =
      M.update (Just . ((-)n)) k mp

chitoi :: NonRedTehai -> Int
chitoi tehai =
  let 
    toitsu = M.size $ M.filter (>= 2) tehai
    kind   = M.size $ M.filter (>= 1) tehai
  in
    6 - toitsu + max (7 - kind) 0

kokushi :: NonRedTehai -> Int
kokushi tehai =
  let
    kind = M.size $ M.filterWithKey (isJihai &&& (>= 1)) tehai
    two  = M.size $ M.filterWithKey (isJihai &&& (>= 2)) tehai
  in
    13 - kind - min 1 two
