{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Sized

import GHC.TypeLits
import Data.Proxy


one :: SizedIO 1 ()
one = wrapWithSize $ putStrLn "one"

two :: SizedIO 2 ()
two = wrapWithSize $ putStrLn "two"

three :: SizedIO 3 ()
three = wrapWithSize $ putStrLn "three"

-- When doing one operation after another,
-- We add the cost of the operations together
six :: SizedIO 6 ()
six = wrapWithExplicitSize (Proxy :: Proxy 0) (putStrLn "sum of: ")
  Sized.>>= \_ -> one
  Sized.>>= \_ -> two
  Sized.>>= \_ -> three
  Sized.>>= \_ -> wrapWithExplicitSize (Proxy :: Proxy 0) $ putStrLn "is six"

t1 = six

-- When doing the Applicative thing (i.e. in parallel), we take the
-- maximum of each side.
t2 :: SizedIO 6 ()
t2 = wrapWithExplicitSize (Proxy :: Proxy 0) (putStrLn "the max of:")
  Sized.>>= \_ -> (\_ _ -> ()) Sized.<$>  three Sized.<*> six
  Sized.>>= \_ -> wrapWithExplicitSize (Proxy :: Proxy 0) (putStrLn "is six!")

-- We can also put an upper bound on the size,
-- which is useful for e.g. development.
t3 :: SizedIO 8 ()
t3 = isAtMost (Proxy :: Proxy 8) t2
   Sized.>>= \_ -> wrapWithSize $ putStrLn "which is less than eight!"


-- Main.hs:56:14: error:
--     • Couldn't match type ‘2’ with ‘1’
--       Expected type: SizedIO 1 ()
--         Actual type: SizedT IO 2 ()
--     • In the expression:
--         wrapWithExplicitSize (Proxy :: Proxy 2) $ putStrLn "two"
--       In an equation for ‘shouldFail’:
--           shouldFail
--             = wrapWithExplicitSize (Proxy :: Proxy 2) $ putStrLn "two"
-- shouldFail :: SizedIO 1 ()
-- shouldFail = wrapWithExplicitSize (Proxy :: Proxy 2) $ putStrLn "two"

main :: IO ()
main = do
  putStrLn "Running t1:"
  runSizedT t1
  putStrLn "Running t2:"
  runSizedT t2
  putStrLn "Running t3:"
  runSizedT t3
