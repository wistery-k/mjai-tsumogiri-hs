{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (lines, length)

import Network
import System.IO
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Control.Monad (mzero)
import Control.Monad.State (StateT, evalStateT, lift, mapStateT)
import Control.Exception (finally)
import Data.Either.Utils (forceEither)
import Data.Functor.Identity (runIdentity)
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec (parseOnly)

import qualified Mjai.Action.SC as SC

import Silica.Silica (Silica)
import qualified Silica.Silica as Silica

data Loop = Continue | Break
  
mainLoop :: Handle -> IO ()
mainLoop sock =
  evalStateT (loopM $ f sock) Silica.init
  where
    loopM :: StateT Silica IO Loop -> StateT Silica IO ()
    loopM m = do 
      b <- m
      case b of
        Continue -> loopM m
        Break    -> return ()
    f :: Handle -> StateT Silica IO Loop
    f h = do
      line <- lift $ hGetLine h
      lift $ hPutStrLn stderr line
      case parse parseJSON . forceEither $ parseOnly json (C.pack line) of
        Success actionSC -> do
          actionCS <- mapStateT liftIdentity $ Silica.silica actionSC
          lift $ case actionSC of
            SC.EndGame -> return Break
            SC.Error _ -> return Break
            _ -> (hPutStrLn h . LC.unpack . encode . toJSON $ actionCS) >> return Continue
        _ -> mzero
    liftIdentity = return . runIdentity
    
main :: IO ()
main = withSocketsDo $ do
  sock <- connectTo "localhost" (PortNumber 11600)
  finally
    (mainLoop sock)
    (hClose sock)