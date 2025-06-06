{- |
nubbb
-}
module Nub (
    nubOrdOn
) where

import qualified Data.Map as M
import Utilities ((||>) , (|>))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

data Trie a = Node Bool ( M.Map a ( Trie a ) ) deriving ( Show )

empty :: Trie a
empty = Node False M.empty

member :: Ord a => [a] -> Trie a -> Bool
member [] (Node isMember _) = isMember
member (x:xs) (Node _ children) = 
    (
        children
        ||> M.lookup x
    )
    <&> member xs
    ||> fromMaybe False

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Node _ children) = Node True children
insert (x:xs) (Node isElem children) = 
    children
    ||> M.alter
            (
                fromMaybe empty 
                |> insert xs 
                |> Just
            )
            x
    |> Node isElem

{- | 
Given a function /f/, \\
and a list /l/, \\
return

> [ x <- l |  all (\\a-\> f a \/= f x) (takeWhile (/=x) l) ] 


Essentially, performing /nub/ by comparing function image value rather than actual value
-}

nubOrdOn :: Ord a => (t -> [a]) -> [t] -> [t]
nubOrdOn f = go empty where
    go       _        [    ] = []
    go alreadyVisited (x:xs) =
        if f x `member`                  alreadyVisited
        then           go                alreadyVisited   (x:xs)
        else x    :    go ( insert (f x) alreadyVisited )    xs