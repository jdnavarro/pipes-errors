module Pipes.Lift.ExceptRT where

import Control.Monad ((>=>))
import Pipes (Proxy, lift)
import Pipes.Lift (distribute)
import Pipes.Internal (unsafeHoist)
import Control.Error
  ( ExceptT(..)
  , ExceptRT(..)
  , runExceptT
  , runExceptRT
  , flipET
  , flipEither
  )
import Pipes.Lift.ExceptT

-- | Turn 'ExceptT' in the base monad into 'ExceptRT'
exceptRP :: Monad m
         => Proxy a' a b' b (ExceptT e m) r
         -> Proxy a' a b' b (ExceptRT r m) e
exceptRP = unsafeHoist lift . runExceptP
       >=> lift . ExceptRT . ExceptT . return
{-# INLINABLE exceptRP #-}

-- | Turn 'ExceptRT' in the base monad into 'ExceptT'
runExceptRP :: Monad m
            => Proxy a' a b' b (ExceptRT r m) e
            -> Proxy a' a b' b (ExceptT e m) r
runExceptRP = unsafeHoist lift . runExceptT . flipET . runExceptRT . distribute
          >=> lift . ExceptT . return . flipEither
{-# INLINABLE runExceptRP #-}
