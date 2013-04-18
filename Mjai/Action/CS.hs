{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Mjai.Action.CS where

import Data.Aeson

import Majong.Pai
import Mjai.Action

data ActionCS =
    None
  | Join      Name Room
  | Dahai     Actor Pai Tsumogiri
  | Reach     Actor
  | Hora      Actor Target Pai
  | Pon       Actor Target Pai Consumed
  | Chi       Actor Target Pai Consumed
  | Ankan     Actor Target Pai Consumed
  | Daiminkan Actor Target Pai Consumed
  | Kakan     Actor Pai
  deriving (Eq, Show)

instance ToJSON ActionCS where
  toJSON (None) = 
    object [ "type" .= ("none"::String) ]
  toJSON (Join name room) = 
    object [ "type" .= ("join"::String),
             "name" .= name,
             "room" .= room ]
  toJSON (Dahai actor pai tsumogiri) = 
    object [ "type"  .= ("dahai"::String),
             "actor" .= actor,
             "pai"   .= pai,
             "tsumogiri" .= tsumogiri ]
  toJSON (Reach actor) =
    object [ "type" .= ("reach"::String),
             "actor" .= actor ]
  toJSON (Hora actor target pai) =
    object [ "type" .= ("hora"::String),
             "actor" .= actor,
             "target" .= target,
             "pai" .= pai ]
  toJSON (Pon actor target pai consumed) =
    object [ "type" .= ("pon"::String),
             "actor" .= actor,
             "target" .= target,
             "pai" .= pai, 
             "consumed" .= consumed ]
  toJSON (Chi actor target pai consumed) =
    object [ "type" .= ("chi"::String),
             "actor" .= actor,
             "target" .= target,
             "pai" .= pai,
             "consumed" .= consumed ]
  toJSON (Ankan actor target pai consumed) =
    object [ "type" .= ("ankan"::String),
             "actor" .= actor,
             "target" .= target,
             "pai" .= pai,
             "consumed" .= consumed ]
  toJSON (Daiminkan actor target pai consumed) =
    object [ "type" .= ("daiminkan"::String),
             "actor" .= actor,
             "target" .= target,
             "pai" .= pai,
             "consumed" .= consumed ]
  toJSON (Kakan actor pai) =
    object [ "type" .= ("kakan"::String),
             "actor" .= actor,
             "pai" .= pai ]
