module Directory (
    FileTree(..),
    isFile,
    isDirectory
) where

import Data.List (transpose, intercalate)
import System.FilePath (addTrailingPathSeparator)
import Utilities ((||>))

-- | A data structure representing the file structure of a directory
-- The tree can be folded or searched through breadth-first search
data FileTree t =
    File { name :: String , path :: t , contents ::String }
    | Directory { name :: String , path :: t , listDir :: [FileTree t] }
    deriving Functor

instance Show (FileTree a) where
    show :: FileTree a -> String
    show ( File      { name = n } ) = n
    show ( Directory { name = n, listDir = l } ) =
        ( n ||> addTrailingPathSeparator ) ++ "\n"
        ++ padUnit ++ "\n"
        ++ unlines ( intercalate [padUnit] $ map ( (pad|++|) . lines . show ) l )
        where
            (|++|) = zipWith (++)
            pad = repeat " " |++| repeat "|" |++| ( "--" : repeat "  " )
            padUnit = pad !! 1

instance Foldable FileTree where
    -- | breadth-first fold on the nodes in a file tree
    foldr :: (a -> b -> b) -> b -> FileTree a -> b
    foldr step base = foldr step base . concat . levels  where
        levels x = [path x] : if isFile x then [] else concat <$> transpose $ levels <$> listDir x

isFile :: FileTree t -> Bool
isFile (File {}) = True
isFile     _     = False

isDirectory :: FileTree t -> Bool
isDirectory (File {}) = False
isDirectory     _     = True