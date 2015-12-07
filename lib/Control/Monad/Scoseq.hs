{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{- |

Scoped sequential identifier generator.

> ghci> runScoseq $ replicateM 10 (identifierToInteger <$> yield)
> [0,1,2,3,4,5,6,7,8,9]

-}

module Control.Monad.Scoseq
  ( -- * Identifier
    Identifier
  , identifierToInteger
    -- * Scoseq
  , MonadScoseq
  , ScoseqT
  , Scoseq
  , runScoseqT
  , runScoseq
  , yield
  ) where

import Data.Proxy
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Ether.State

newtype Identifier s = Identifier Integer
  deriving (Eq, Ord, Show)

identifierToInteger :: Identifier s -> Integer
identifierToInteger (Identifier n) = n

type Gen = Const [Integer]

gen :: Integer -> Gen s
gen n = Const [n..]

-- | Empty type used as an Ether tag.
data SCOSEQ

-- | Proxy value for `SCOSEQ`.
scoseq :: Proxy SCOSEQ
scoseq = Proxy

-- | A class of monads that can yield unique identifiers.
type MonadScoseq s = MonadState SCOSEQ (Gen s)

-- | A monad transformer that can yield unique identifiers.
type ScoseqT s = StateT SCOSEQ (Gen s)

-- | A monad that can yield unique identifiers.
type Scoseq s = State SCOSEQ (Gen s)

-- | Run `ScoseqT`.
runScoseqT :: Monad m => (forall s . ScoseqT s m a) -> m a
runScoseqT m = evalStateT scoseq m (gen 0)

-- | Run `Scoseq`.
runScoseq :: (forall s . Scoseq s a) -> a
runScoseq m = runIdentity (runScoseqT m)

-- | Yield a unique identifier.
yield :: MonadScoseq s m => m (Identifier s)
yield = do
  Const (n:ns) <- get scoseq
  put scoseq (Const ns)
  return (Identifier n)
