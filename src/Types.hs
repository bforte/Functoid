{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Types
  ( module Types
  , get, modify, use
  , (&), (^.), (.~), (.=), (%=)
  ) where

import LambdaCalc

import Data.Array
import Control.Lens
import Control.Monad.State
import Prelude hiding (exp)


-- Monad transformer for Lambdoid computations
newtype LC a = LC { unLC :: StateT Env IO a }
  deriving (Functor, Applicative, Monad, MonadState Env, MonadIO)

runProg :: Env -> LC a -> IO Env
runProg s c = snd <$> runStateT (unLC c) s

type Prog = Array (Int,Int) Char

data Direction = L | R | U | D
  deriving (Eq,Enum,Show)

data Env = Env
  { _prog :: Prog
  , _args :: [Exp]
  , _exp  :: Exp
  , _pos  :: (Int,Int)
  , _dir  :: Direction
  } deriving Show

data Opts = Opts
  { _expr    :: Bool
  , _verbose :: Bool
  , _help    :: Bool
  }


makeLenses ''Env
makeLenses ''Opts
