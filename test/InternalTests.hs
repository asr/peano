{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Peano  ( Nat )
import System.Exit ( exitFailure, exitSuccess )

import Test.QuickCheck
  ( (==>)
  , Property
  , quickCheckResult
  , Result(Success)
  , Testable
  )

------------------------------------------------------------------------------
-- Auxiliary functions

-- From Agda.
-- Monadic if-then-else.
ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM c m m' =
    do b ← c
       if b then m else m'

-- From Agda.
isSuccess ∷ Result → Bool
isSuccess Success{} = True
isSuccess _         = False

quickCheck' :: Testable prop ⇒ prop → IO Bool
quickCheck' p = fmap isSuccess $ quickCheckResult p

-- From Agda.
runTests ∷ String    -- ^ A label for the tests. Used for
                      --   informational purposes.
         → [IO Bool]
         → IO Bool
runTests name tests = do
  putStrLn name
  and <$> sequence tests

------------------------------------------------------------------------------
-- Properties

-- From:
-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/libraries/base-4.7.0.2/Prelude.html#v:signum
prop_signum ∷ Nat → Bool
prop_signum x = abs x * signum x == x

-- In non-negative integers @div@ and @quot@ should be have the same
-- behaviour.
prop_div_quot ∷ Nat → Nat → Property
prop_div_quot n d = n >= 0 && d > 0 ==> n `div` d == n `quot` d

------------------------------------------------------------------------------
-- All tests

allTests :: IO Bool
allTests = runTests "Internal Tests"
  [ quickCheck' prop_div_quot
  , quickCheck' prop_signum
  ]

main ∷ IO ()
main = ifM allTests exitSuccess exitFailure
