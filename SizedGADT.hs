{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SizedGADT where

import GHC.TypeLits
import Data.Type.Bool
import Unsafe.Coerce
import Data.Proxy
import Data.Type.Equality

-- Type level Maximum function
type family Max (a :: Nat) (b :: Nat) where
  Max a b = If (CmpNat a b == 'LT) b a

infixl 1  |>>, |>>=
infixl 4 |<*>, |<$>

class KnownNat s => SizedFunctor f s where
  fmap :: (a -> b) -> f s a -> f s b

-- | A synonym for fmap
(|<$>) :: SizedFunctor f s =>  (a -> b) -> f s a -> f s b
(|<$>) = SizedGADT.fmap

class (SizedFunctor f s) => SizedApplicative f s  where
  pure :: a -> f 0 a
  (|<*>) :: (
    SizedApplicative f t,
    SizedApplicative f v,
    s ~ Max t v) => f t (a -> b) -> f v a -> f s b


class (KnownNat s, SizedApplicative m s) => SizedMonad m s where
  return :: SizedMonad m s => a -> m 0 a
  (|>>=) :: SizedMonad m t => m s a -> (a -> m t b) -> m (s + t) b

(|>>) :: (SizedMonad m s, SizedMonad m t) => m s a -> m t b -> m (s + t) b
a |>> b = a |>>= (\_ -> b)


-- data SizedT m (n :: Nat) a where
--     Op :: Monad m => Nat -> m a -> a -> SizedT m n a
--     NoOp :: Monad m => m a -> a -> SizedT m 0 a

data SizedT m (n :: Nat) a where
    Op :: m a -> SizedT m n a
    NoOp :: m a -> SizedT m 0 a

-- stM :: SizedT m (n :: Nat) a -> m a
-- stM (Op m _) = m
-- stM (NoOp m) = m

-- stN :: SizedT m s a -> Nat
-- stN (NoOp _) = 0
-- stN (Op _ n) = n

wrapWithSize :: (0 <= n) =>  f a -> SizedT f n a
wrapWithSize m = Op m

wrapWithExplicitSize :: KnownNat n => Proxy n -> f a -> SizedT f n a
wrapWithExplicitSize _ m = Op m

isAtMost :: (KnownNat n, KnownNat m, n <= m) => Proxy m -> SizedT f n a -> SizedT f m a
isAtMost _ (NoOp m) = Op m
isAtMost _ (Op m) = Op m

-- unwrap :: SizedT f n a -> f a
-- unwrap = unsafeCoerce

-- wrap :: (0 <= n) => f a -> SizedT f n a
-- wrap = unsafeCoerce

instance (KnownNat n, Functor f) => SizedFunctor (SizedT f) n where
  fmap f (Op m) = Op $ Prelude.fmap f m
  fmap f (NoOp m) = NoOp $ Prelude.fmap f m


instance (KnownNat n, Applicative f) => SizedApplicative (SizedT f) n where
  pure = NoOp . Prelude.pure
  (NoOp ma) |<*> (NoOp mb) = NoOp (ma <*> mb)
  (Op ma) |<*> (NoOp mb) = Op (ma <*> mb)
  (NoOp ma) |<*> (Op mb) = Op (ma <*> mb)
  ((Op ma) :: SizedT m na (a -> b)) |<*> ((Op mb) :: SizedT m nb a) = Op (ma <*> mb)  :: SizedT m (Max na nb) b

instance (KnownNat n, Monad m) => SizedMonad (SizedT m) n where
  return = NoOp . Prelude.return
  (Op ma) |>>= mb = Op $ ma >>= (\k -> case mb k of
    NoOp m -> m
    Op m -> m)
  -- We want NoOp >>= (\k -> NoOp m) to be NoOp though
  (NoOp ma) |>>= mb = Op $ ma >>= (\k -> case mb k of
    NoOp m -> m
    Op m -> m)

type SizedIO = SizedT IO

runSizedT :: SizedT m n a -> m a
runSizedT (Op m) = m
runSizedT (NoOp m) = m
  
  -- (Op $ ma >>= (\k -> case fmb k of
  --   NoOp mb -> mb
  --   Op mb -> mb)) :: SizedT m t b



--   a |>>= b = wrapWithSize $ (runSizedT a) >>= (\k -> runSizedT $ b k)
