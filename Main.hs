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

module Main where

import Sized
import Data.Proxy
import qualified Prelude as P
import Prelude hiding ((>>=), (>>), return, (<*>), (<$>), fmap, pure)



one :: SizedIO 1 ()
one = wrapWithSize $ putStrLn "one"

two :: SizedIO 2 ()
two = wrapWithSize $ putStrLn "two"

three :: SizedIO 3 ()
three = wrapWithSize $ putStrLn "three"

-- When doing one operation after another,
-- We add the cost of the operations together
six :: SizedIO 3 ()
six = do wrapWithExplicitSize (Proxy :: Proxy 0) (putStrLn "sum of: ")
         one
         two
         three
         wrapWithExplicitSize (Proxy :: Proxy 0) $ putStrLn "is six"

t1 = six

-- When doing the Applicative thing (i.e. in parallel), we take the
-- maximum of each side.
t2 :: SizedIO 6 ()
t2 = do wrapWithExplicitSize (Proxy :: Proxy 0) (putStrLn "the max of:")
        three
        six
        -- (\_ _ -> ()) <$> three <*> six
        wrapWithExplicitSize (Proxy :: Proxy 0) (putStrLn "is six!")

-- We can also put an upper bound on the size,
-- which is useful for e.g. development.
t3 :: SizedIO 8 ()
t3 = do isAtMost (Proxy :: Proxy 8) t2
        wrapWithSize $ putStrLn "which is less than eight!"


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
main = runSizedT t3
-- main :: IO ()
-- main = do
--   putStrLn "Running t1:"
--   runSizedT t1
--   putStrLn "Running t2:"
--   runSizedT t2
--   putStrLn "Running t3:"
--   runSizedT t3
--   where (>>) = (P.>>) :: IO a -> IO b -> IO b
--         (>>=) = (P.>>=) :: IO a -> (a -> IO b) -> IO b 
--         (return) = P.return :: a -> IO a
--         -- (join) = P.join :: IO (IO a) -> IO a
