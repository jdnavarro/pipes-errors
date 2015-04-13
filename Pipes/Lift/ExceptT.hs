module Pipes.Lift.ExceptT where

import Control.Monad ((>=>))
import Pipes (Proxy, lift)
import Pipes.Lift (distribute)
import Pipes.Internal (unsafeHoist)
import Control.Error (ExceptT(..), runExceptT, flipEither)
import Pipes.Lift.Error.Instances ()

-- | Wrap the base monad in 'ExceptT'.
exceptP :: Monad m
        => Proxy a' a b' b m (Either e r)
        -> Proxy a' a b' b (ExceptT e m) r
exceptP = unsafeHoist lift >=> lift . ExceptT . return
{-# INLINABLE exceptP #-}

-- | Run 'ExceptT' in the base monad.
runExceptP :: Monad m
           => Proxy a' a b' b (ExceptT e m) r
           -> Proxy a' a b' b m (Either e r)
runExceptP = runExceptT . distribute
{-# INLINABLE runExceptP #-}

-- | Flip the type variables in the 'ExceptT' base monad.
flipEP :: Monad m
       => Proxy a' a b' b (ExceptT a m) b
       -> Proxy a' a b' b (ExceptT b m) a
flipEP = unsafeHoist lift . runExceptT . distribute
     >=> lift . ExceptT . return . flipEither
{-# INLINABLE flipEP #-}
