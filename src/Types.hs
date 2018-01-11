{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Types
  ( module Types
  , get, modify, use, view
  , (&), (^.), (.~), (.=), (%=)
  ) where

import LambdaCalc

import Data.Array
import Control.Monad.Reader
import Control.Monad.State.Strict
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH


-- | Monad transformer for Functoid computations
newtype FC a = FC { unFC :: StateT Env (ReaderT Flags IO) a }
  deriving (Functor, Applicative, Monad, MonadState Env, MonadReader Flags, MonadIO)

-- | Execute a Functoid computation
execProg :: Env -> Flags -> FC a -> IO Env
execProg s r c = runReaderT (execStateT (unFC c) s) r

type Prog = Array (Int,Int) Char

data Direction = L | R | U | D
  deriving (Eq,Enum,Show)

-- | State environment used for evaluation
data Env = Env
  { _prog :: !Prog      -- ^ Functoid source
  , _args :: [Exp]      -- ^ Arguments passed to interpreter
  , _exp  :: Exp        -- ^ Current 'Exp'
  , _pos  :: !(Int,Int) -- ^ Current position
  , _dir  :: !Direction -- ^ Current travel direction
  } deriving Show

-- | Strict version of (%=)
infix 4 %!=

(%!=) :: ASetter Env Env a b -> (a -> b) -> FC ()
l %!= f = modify' (l %~ f)


-- | Reader type to keep track of different execution modes
data Flags = Flags
  { _clear   :: !Bool  -- ^ Clear the current 'Exp' when printing
  , _exit    :: !Bool  -- ^ Exit as soon as print statement taken
  , _force   :: !Bool  -- ^ Always force simplifying
  , _quiet   :: !Bool  -- ^ Dont' print the final expression
  , _verbose :: !Bool  -- ^ Be verbose (print steps taken)
  }


-- | Command-line arguments
data Opts = Opts
  { _expr      :: Bool   -- ^ Pass source as command-line argument
  , _flags     :: Flags  -- ^ Execution flags
  , _transform :: Bool   -- ^ Transform lambda expression to SKIBCW
  }

-- | By default: clear when evaluating, keep going, be lazy, and keep quiet
defaults :: Opts
defaults = Opts False (Flags True False False False False) False


makeLenses ''Env
makeLenses ''Flags
makeLenses ''Opts
