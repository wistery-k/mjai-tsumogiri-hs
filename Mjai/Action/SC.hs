{-# LANGUAGE OverloadedStrings #-}
module Mjai.Action.SC where

import Data.Aeson hiding (Error)
import Data.Aeson.Types hiding (Error)

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)

import Majong.Pai
import Mjai.Action

data ActionSC = 
    Hello         Protocol ProtocolVersion
  | StartGame     Id Names
  | StartKyoku    Bakaze Kyoku Honba Kyotaku Oya DoraMarker Tehais
  | Tsumo         Actor Pai
  | Dahai         Actor Pai Tsumogiri
  | Reach         Actor
  | ReachAccepted Actor Deltas Scores
  | Pon           Actor Target Pai Consumed
  | Chi           Actor Target Pai Consumed
  | Daiminkan     Actor Target Pai Consumed
  | Kakan         Actor Pai
  | Ankan         Actor Target Pai Consumed
  | Hora          Actor Target Pai HoraTehais UradoraMarkers Yakus Fu Fan HoraPoints Deltas Scores
  | Ryukyoku      String Tehais Tenapais Deltas Scores
  | EndKyoku
  | EndGame
  | Error         Message
  deriving (Eq, Show)

instance FromJSON ActionSC where
  parseJSON (Object v) = do
    typ <- v .: "type" :: Parser String
    case typ of
      "hello" ->
        Hello
        <$> v .: "protocol"
        <*> v .: "protocol_version"    
      "start_game" ->
        StartGame
        <$> v .: "id"
        <*> v .: "names"
      "start_kyoku" ->
        StartKyoku
        <$> v .: "bakaze"
        <*> v .: "kyoku"
        <*> v .: "honba"
        <*> v .: "kyotaku"
        <*> v .: "oya"
        <*> v .: "dora_marker"
        <*> v .: "tehais"
      "tsumo" ->
        Tsumo
        <$> v .: "actor"
        <*> v .: "pai"
      "dahai" ->
        Dahai
        <$> v .: "actor"
        <*> v .: "pai"
        <*> v .: "tsumogiri"
      "reach" ->
        Reach
        <$> v .: "actor"
      "reach_accepted" ->
        ReachAccepted
        <$> v .: "actor"
        <*> v .: "deltas"
        <*> v .: "scores"
      "pon" ->
        Pon
        <$> v .: "actor"
        <*> v .: "target"
        <*> v .: "pai"
        <*> v .: "consumed"
      "chi" ->
        Chi
        <$> v .: "actor"
        <*> v .: "target"
        <*> v .: "pai"
        <*> v .: "consumed"
      "daiminkan" ->
        Daiminkan
        <$> v .: "actor"
        <*> v .: "target"
        <*> v .: "pai"
        <*> v .: "consumed"
      "kakan" ->
        Kakan
        <$> v .: "actor"
        <*> v .: "pai"
      "ankan" ->
        Ankan
        <$> v .: "actor"
        <*> v .: "target"
        <*> v .: "pai"
        <*> v .: "consumed"
      "hora" ->
        Hora
        <$> v .:  "actor"
        <*> v .:  "target"
        <*> v .:  "pai"
        <*> v .:  "hora_tehais"
        <*> v .:? "uradora_markers"
        <*> v .:  "yakus"
        <*> v .:  "fu"
        <*> v .:  "fan"
        <*> v .:  "hora_points"
        <*> v .:  "deltas"
        <*> v .:  "scores"
      "ryukyoku" ->
        Ryukyoku
        <$> v .: "reason"
        <*> v .: "tehais"
        <*> v .: "tenpais"
        <*> v .: "deltas"
        <*> v .: "scores"
      "end_kyoku" ->
        return EndKyoku
      "end_game" ->
        return EndGame
      "error" ->
        Error
        <$> v .: "message"
      _ -> 
        mzero
  parseJSON _ = mzero
