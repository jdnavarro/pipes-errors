{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pipes.Lift.EitherT where

import Control.Monad ((>=>))
import Pipes (Proxy, MFunctor, hoist, lift)
import Pipes.Lift (distribute)
import Pipes.Internal (unsafeHoist)
import Control.Error (EitherT(..), runEitherT, flipE)

instance MFunctor (EitherT e) where
    hoist nat m = EitherT (nat (runEitherT m))

-- | Wrap the base monad in 'EitherT'.
eitherP :: Monad m
        => Proxy a' a b' b m (Either e r)
        -> Proxy a' a b' b (EitherT e m) r
eitherP = unsafeHoist lift >=> lift . EitherT . return
{-# INLINABLE eitherP #-}

-- | Run 'EitherT' in the base monad.
runEitherP :: Monad m
           => Proxy a' a b' b (EitherT e m) r
           -> Proxy a' a b' b m (Either e r)
runEitherP = runEitherT . distribute
{-# INLINABLE runEitherP #-}

-- | Flip the type variables in the 'EitherT' base monad.
flipEP :: Monad m
       => Proxy a' a b' b (EitherT a m) b
       -> Proxy a' a b' b (EitherT b m) a
flipEP = unsafeHoist lift . runEitherT . distribute
     >=> lift . EitherT . return . flipE
{-# INLINABLE flipEP #-}
