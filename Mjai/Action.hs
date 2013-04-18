module Mjai.Action where

import Prelude hiding (head)

import Control.Monad (mzero)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char (digitToInt)
import Data.Text (pack, unpack, head)

import Data.Aeson

import Majong.Suit
import Majong.Pai
import Majong.Wind

instance FromJSON Pai where
  parseJSON (String t) =
    case unpack t of
      [c]     -> return $ Pai Jihai (jihaiJson c) False
      [n,s]   -> return $ Pai (suitJson s) (digitToInt n) False
      [n,s,_] -> return $ Pai (suitJson s) (digitToInt n) True
      _       -> mzero
    where
      suitJson 'm' = Manzu
      suitJson 'p' = Pinzu
      suitJson 's' = Sozu
      suitJson _   = undefined
      jihaiJson c = 1 + (fromJust $ elemIndex c "ESWNPFC")
  parseJSON _ = mzero

instance ToJSON Pai where
  toJSON pai = String (pack (show pai))

instance FromJSON Wind where
  parseJSON (String t) =
    return $ fromJust $ lookup (head t) [('E', Ton), ('S', Nan), ('W', Xia), ('N', Pei)]
  parseJSON _ = mzero

type Protocol = String
type ProtocolVersion = Int
type Id = Int
type Names = [String]
type Bakaze = Wind
type Kyoku = Int
type Honba = Int
type Kyotaku = Int
type Oya = Int
type DoraMarker = Pai
type Tehais = [[Pai]]
type Deltas = [Int]
type Scores = [Int]
type HoraTehais = [Pai]
type UradoraMarkers = Maybe [Pai]
type Yakus = [(String, Int)]
type Fu = Int
type Fan = Int
type HoraPoints = Int
type Reason = String
type Tenapais = [Bool]
type Message = String
type Name = String
type Room = String
type Actor = Int
type Target = Int
type Tsumogiri = Bool
type Consumed = [Pai]
