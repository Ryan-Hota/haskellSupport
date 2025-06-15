module Utilities where

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

hello :: b -> String
hello = const "world"