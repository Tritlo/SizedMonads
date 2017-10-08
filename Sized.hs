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

module Sized where

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
(|<$>) = Sized.fmap

class (SizedFunctor f s) => SizedApplicative f s  where
  pure :: a -> f s a
  (|<*>) :: (SizedApplicative f t, SizedApplicative f v, s ~ Max t v) => f t (a -> b) -> f v a -> f s b


class (KnownNat s, SizedApplicative m s) => SizedMonad m s where
  return :: SizedMonad m s => a -> m s a
  (|>>=) :: SizedMonad m t => m s a -> (a -> m t b) -> m (s + t) b

(|>>) :: (SizedMonad m s, SizedMonad m t) => m s a -> m t b -> m (s + t) b
a |>> b = a |>>= (\_ -> b)

data Vec n a where
  Nil :: Vec 0 a
  (:-) :: KnownNat n => a -> Vec n a -> Vec (n + 1) a

instance KnownNat n => SizedFunctor Vec n where
  fmap _ Nil = Nil
  fmap f (a :- as) = (f a) :- (Sized.fmap f as)


-- Type level flip, taken from https://github.com/GU-CLASP/TypedFlow/blob/8604312e4c36533b6b7dae7117378ca44bef7fbf/TypedFlow/Types.hs#L215
newtype Flip g t s = Flip {fromF :: g s t}
newtype Flip2 g t s v = Flip2 {fromF2 :: g s t v}

-- | We write a simple "transformer" (i.e MonadTrans (Flip2 SizedT n))
-- to wrap a non sized monad.
newtype SizedT m (n :: Nat) a = SizedT { runSizedT :: m a}

wrapWithSize :: (0 <= n) =>  f a -> SizedT f n a
wrapWithSize = unsafeCoerce

wrapWithExplicitSize :: KnownNat n => Proxy n -> f a -> SizedT f n a
wrapWithExplicitSize _ = wrapWithSize

isAtMost :: (KnownNat n, KnownNat m, n <= m) => Proxy m -> SizedT f n a -> SizedT f m a
isAtMost _ = unsafeCoerce

instance (KnownNat n, Functor f) => SizedFunctor (SizedT f) n where
  fmap f a = wrapWithSize (Prelude.fmap f (runSizedT a))

instance (KnownNat n, Applicative f) => SizedApplicative (SizedT f) n where
  pure = wrapWithSize . Prelude.pure
  a |<*> b = wrapWithSize $ (runSizedT a) <*> (runSizedT b)

instance (KnownNat n, Monad m) => SizedMonad (SizedT m) n where
  return = wrapWithSize . Prelude.return
  a |>>= b = wrapWithSize $ (runSizedT a) >>= (\k -> runSizedT $ b k)


type SizedIO = SizedT IO


