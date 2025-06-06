module Utilities (
    (||>),
    (|>),
    ifThenElse,
    safeHead,
    safeWhen,
    fromMaybe,
    -- genFilterM
) where

import Control.Monad (mfilter, filterM)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Applicative (Alternative (..), asum)
import Data.Foldable (toList)

-- | given a value and a function, apply the function to the value
infixl 0 ||>
(||>) :: a -> (a -> b) -> b
(||>) = flip ($)

-- | sequence two computations\\
-- /which is the same as/\\
-- composition of two functions in a more intuitive manner
infixl 9 |>
(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

-- | @ \\ b x y -> if b then x else y@
ifThenElse :: Bool -> p -> p -> p
ifThenElse True = \ x _ -> x
ifThenElse  _   = \ _ y -> y

-- @ \\ b x y -> if b then x else y @ \\
-- but where b, x, y are monadic
-- ifThenElseM :: Monad m => m Bool -> m b -> m b -> m b
-- ifThenElseM mb mx my = mb >>= ( \ b -> if b then mx else my )

-- | The @safeHead@ function returns @Nothing@ on an empty list or @Just a@ where @a@ is the first element of the list.
safeHead :: [a] -> Maybe a
safeHead = listToMaybe

-- | given a boolean valued checking function,//
-- check whether an object passes the check or not,//
-- returning Just object if passed, and Nothing if not//
safeWhen :: (a -> Bool) -> a -> Maybe a
safeWhen = mfilter |> ( pure |> )

-- | filterM, generaliized to any container, not just []
genFilterM :: (Foldable t, Applicative m, Alternative t) => (a -> m Bool) -> t a -> m (t a)
genFilterM predicate =
    toList
    |> filterM predicate
    |> fmap ( map pure |> asum )