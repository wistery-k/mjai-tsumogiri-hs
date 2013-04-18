{-# LANGUAGE RecordWildCards #-}
module Silica.Silica where

import Prelude hiding (id, init)

import Control.Monad.State

import Majong.Shanten

import qualified Mjai.Action.SC as SC
import qualified Mjai.Action.CS as CS

data Silica = Silica { id :: Int }
init :: Silica
init = Silica { id = -1 }

silica :: Monad m => SC.ActionSC -> StateT Silica m CS.ActionCS

silica (SC.Hello _ _) =
  return $ CS.Join "silica" "default"

silica (SC.StartGame id _) = do
  modify (\s -> s { id = id })
  return CS.None

silica (SC.Tsumo actor pai) = do
  Silica{..} <- get
  return $
    if actor == id then 
      CS.Dahai id pai True
    else
      CS.None

silica _ =
  return CS.None
