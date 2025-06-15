module Target_IO (
    Target,
    targetPath,
    takeAsTarget
) where

import FilePath (AbsoluteFilePath, RootRelativeFilePath, relativizeToRoot)

{- |
Denotes the haskell program file meant to be compiled

use @takeAsTarget@ to make an object of this type
-}
newtype Target = Target RootRelativeFilePath deriving Show

{- |
@Target@ denotes the haskell program file meant to be compiled

use @takeAsTarget@ to declare some path as the path to your /target/
-}
takeAsTarget :: AbsoluteFilePath -> IO Target
takeAsTarget path = Target <$> relativizeToRoot path

{- |
@Target@ denotes the haskell program file meant to be compiled

use @relativeTarget@ to obtain the filePath of the target relative to the root
-}
targetPath :: Target -> RootRelativeFilePath
targetPath (Target path) = path