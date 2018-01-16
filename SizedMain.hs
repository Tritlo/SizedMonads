{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sized
import Data.Proxy
import qualified Prelude as P
-- import Prelude hiding ((>>=), (>>), return, (<*>), (<$>), fmap, pure)
import GHC.TypeLits
import Prelude (fail, Functor, IO, ($), putStrLn, id)


fmap :: (SizedFunctor f) => (a -> b) -> f i a -> f i b
fmap = sfmap
(<$>) :: (SizedFunctor f) => (a -> b) -> f i a -> f i b
(<$>) = fmap


(<*>) ::  (SizedApplicative f, k ~ Max i j) => f i (a -> b) -> f j a -> f k b
(<*>) = (|<*>)
pure :: (SizedApplicative f) => a -> f 0 a
pure = spure

return :: SizedMonad m => a -> m 0 a
return = sreturn
(>>=) :: SizedMonad m => m i a -> (a -> m j b) -> m (i + j) b
(>>=) = (|>>=)

(*>) :: (SizedApplicative f) => f s a -> f 0 b -> f s b
(*>) = (|*>)

-- For regular applicatives, (>>) = (*>). But for us,
-- (>>) :: (SizedMonad m) => m i a -> m j b -> m (i+j) b
-- but
-- (*>) :: (SizedApplicative f) => f s a -> f 0 b -> f s b
-- If ApplicativeDo is on, it desugars to do {one (); two (); return ()} to
-- fmap (\_ _ -> return ()) <*> (one () >> return ()) <*> (two () >> return ())
-- using -- (>>) and not (*>), as it should. I.e. the expected output should be
-- fmap (\_ _ -> return ()) <*> (one () *> pure ()) <*> (two () *> pure ())
-- It usually works, but in our case (|*>) and (|>>) aren't actually equivalent!
(>>) = (*>)

-- This works, but
t1a :: SizedIO 3 ()
t1a = one () |>> two ()

t1b :: SizedIO 2 ()
t1b = do {one (); two (); return ();}

-- This is wrong
-- t1b :: SizedIO 3 ()
-- t1b = one () >> two ()

one :: () -> SizedIO 1 ()
one _ = wrapWithCost (Proxy :: Proxy 1) $ putStrLn "one" 
two :: () -> SizedIO 2 ()
two _ = wrapWithCost (Proxy :: Proxy 2) $ putStrLn "two"

three :: () -> SizedIO 3 ()
three _ = wrapWithCost (Proxy :: Proxy 3) $ putStrLn "three"



t1 :: SizedIO 6 ((),(),())
t1 = do { x <- one ();
          y <- two x;
          z <- three y;
          return (x,y,z);
        }

t2 :: SizedIO 3 ((),(),())
t2 = do {x <- one ();
         y <- two ();
         z <- three ();
         return (x,y,z)
         }

t3 :: SizedIO 5 ((), (), ())
t3 = isAtMost (Proxy :: Proxy 5) t2


t4 :: SizedIO 3 ()
t4 = do { one (); two (); three (); return ()}

-- Desugars to
{- t4 =
  <*>
    $dSizedApplicative_a168
    ($d~_a28U `cast` ...)
    (<*>
       $dSizedApplicative_a168
       ($d~_a28P `cast` ...)
       (fmap
          $dSizedFunctor_a15J
          (\ ds_d2k7 ds_d2k8 ds_d2k9 ->
             case ds_d2k7 of _ { () ->
             case ds_d2k8 of _ { () -> case ds_d2k9 of _ { () -> () } }
             })
          ((>>
              $dSizedMonad_a15s
              ($f~kab (Eq# @~ ...))
              $dKnownNat_a17e
              (one ())
              (return $fMonadSizedT ()))
           `cast` ...))
       ((>>
           $dSizedMonad_a15s
           ($f~kab (Eq# @~ ...))
           $dKnownNat_a17e
           (two ())
           (return $fMonadSizedT ()))
        `cast` ...))
    ((>>
        $dSizedMonad_a15s
        ($f~kab (Eq# @~ ...))
        $dKnownNat_a17e
        (three ())
        (return $fMonadSizedT ()))
     `cast` ...) 
-}

--
-- -- This one doesn't fail!
-- t4 :: SizedIO 3 ()
-- t4 = do { _ <- one (); _ <- two (); _ <- three (); return ()}
--
-- The latter desugars to
{-
t4 =
  <*>
    $dSizedApplicative_a15d
    ($d~_a18H `cast` ...)
    (<*>
       $dSizedApplicative_a15d
       ($d~_a18K `cast` ...)
       (fmap $dSizedFunctor_a14O (\ _ _ _ -> ()) (one ()))
       (two ()))
    (three ())
-}

void :: Functor f => f a -> f ()
void x = () P.<$ x

main :: IO ()
main = void $ runSizedT $ t4

