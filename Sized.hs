{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sized where

import GHC.TypeLits
import Data.Type.Bool
import Data.Proxy
import Data.Type.Equality
import Debug.Trace

-- Type level Maximum function
type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a 0 = a
  Max 0 b = b
  Max a b = If (a <=? b) b a


class SizedFunctor f where
  sfmap :: (a -> b) -> f i a -> f i b
  (|<$) :: a -> f i b -> f i a
  (|<$) = sfmap . const

(|<$>) :: (SizedFunctor f) => (a -> b) -> f i a -> f i b
f |<$> b = sfmap f b

class (SizedFunctor f) => SizedApplicative f where
  spure :: a -> f 0 a
  (|<*>) ::  (k ~ Max i j) => f i (a -> b) -> f j a -> f k b

  -- You can only sequence free stuff
  -- This type is a bit weird, but without it (and then defining
  -- (>>) as (|*>)), you get a lot of ambigous type variables from
  -- ApplicativeDo.
  (|*>) :: f i a -> f 0 b -> f i b
  a |*> b = (id |<$ a) |<*> b

class SizedApplicative m  => SizedMonad m  where
  (|>>=) ::  m i a -> (a -> m j b) -> m (i + j)  b

  sreturn :: a -> m 0 a
  sreturn = spure

  join :: m i (m j a) -> m (i + j)  a
  join m = m |>>= id

  -- This should never be used, since
  -- we never want to sequence without dependencies
  -- those should only be done with appliatives.
  -- but we have this here to allow explicit forcing of sequencing
  -- Use (*>) to express sequencing.
  (|>>) :: m i a -> m j b -> m (i+j) b
  a |>> b = a |>>= (\_ -> b)



data SizedT m (i :: Nat)  a where
  Op :: Proxy i -> m a -> SizedT m i a
  NoOp :: m a -> SizedT m 0 a

wrapWithSize :: m a -> SizedT m 0 a
wrapWithSize = NoOp

wrapWithCost :: (0 <= i) => Proxy i -> m a -> SizedT m i a
wrapWithCost = Op

isAtMost :: (n <= i) => Proxy i -> SizedT m n a -> SizedT m i a
isAtMost x (Op _ a) = Op x a
isAtMost x (NoOp a) = Op x a

runSizedT :: SizedT m i a -> m a
runSizedT (Op _ m) = m
runSizedT (NoOp m) = m


instance Functor f => SizedFunctor (SizedT f) where
  sfmap f (Op x a) = Op x (fmap f a)
  sfmap f (NoOp a) = NoOp (fmap f a)

instance Applicative f => SizedApplicative (SizedT f) where
  spure = NoOp . pure
  -- (NoOp a) |*> (NoOp b) = NoOp (a *> b)
  -- (Op x a) |*> (NoOp b) = Op x (a *> b)
  -- (NoOp a) |*> (Op y b) = NoOp (a *> b)
  -- (Op (x :: Proxy i) a) |*> (Op (y :: Proxy j) b) = Op x (a *> b)

  (NoOp a) |<*> (NoOp b) = NoOp (a <*> b)
  (NoOp a) |<*> (Op y b) = Op y (a <*> b)
  (Op x a) |<*> (NoOp b) = Op x (a <*> b)
  (Op (x :: Proxy i) a) |<*> (Op (y :: Proxy j) b) = Op (Proxy :: Proxy (Max i j)) (a <*> b)


instance Monad m => SizedMonad (SizedT m) where
  (Op (Proxy :: Proxy i) a) |>>= (m :: a -> SizedT m j b) = Op (Proxy :: Proxy (i + j)) (a >>= (\k -> runSizedT $ m k))
  (NoOp a) |>>= (m :: a -> SizedT m j b) = Op (Proxy :: Proxy i) (a >>= (\k -> runSizedT $ m k))



-- All SizedT are functors for KnownNats
instance  (KnownNat t, Functor f) => Functor (SizedT f t)  where
  fmap = sfmap

-- 0 has instances for all of these, since  Max 0 0 = 0 and (0+0) = 0
instance Applicative f => Applicative (SizedT f 0)  where
  pure = pure
  (<*>) = (<*>)

instance Monad m => Monad (SizedT m 0) where
    return = sreturn
    (>>=) = (|>>=)


type SizedIO = SizedT IO


