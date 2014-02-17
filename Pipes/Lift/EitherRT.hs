{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pipes.Lift.EitherRT where

import Control.Monad ((>=>))
import Pipes (Proxy, MFunctor, lift, hoist)
import Pipes.Lift (distribute)
import Pipes.Internal (unsafeHoist)
import Control.Error
  ( EitherT(..)
  , EitherRT(..)
  , runEitherT
  , runEitherRT
  , flipET
  , flipE
  )
import Pipes.Lift.EitherT

instance MFunctor (EitherRT r) where
    hoist nat m = EitherRT (hoist nat (runEitherRT m))

-- | Turn 'EitherT' in the base monad into 'EitherRT'
eitherRP :: Monad m
         => Proxy a' a b' b (EitherT e m) r
         -> Proxy a' a b' b (EitherRT r m) e
eitherRP = unsafeHoist lift . runEitherP
       >=> lift . EitherRT . EitherT . return
{-# INLINABLE eitherRP #-}

-- | Turn 'EitherRT' in the base monad into 'EitherT'
runEitherRP :: Monad m
            => Proxy a' a b' b (EitherRT r m) e
            -> Proxy a' a b' b (EitherT e m) r
runEitherRP = unsafeHoist lift . runEitherT . flipET . runEitherRT . distribute
          >=> lift . EitherT . return . flipE
{-# INLINABLE runEitherRP #-}
