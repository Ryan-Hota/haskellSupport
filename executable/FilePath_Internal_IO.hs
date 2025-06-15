module FilePath_Internal_IO (
    AssuredToBe(..),
    unWrap,
    Absolutable(..),
    Absolute(..),
    AbsoluteFilePath,
    makeAbsolute,
    RootRelativeFilePath,
    rootOf,
    relativizeToRoot
) where

import qualified System.FilePath as F
import Utilities ((|>), (||>))
import Data.List (scanl')
import qualified System.Directory as D
import Control.Monad (filterM)
import Data.Maybe ( listToMaybe, fromMaybe )

{- |
@AssuredToBe@ is a wrapper meant to give type level assurance that the @FilePath@ associated with @AssuredToBe pathType@ is of type @pathType@

For exmaple, @AssuredToBe Absolute@ is meant to guarantee that the associated @FilePath@ is absolute
-}
data AssuredToBe pathType = AssuredToBe pathType FilePath deriving Show

-- | extract the associated file path from an object wrapped with the @AssuredToBe@ wrapper
unWrap :: Absolutable pathType => AssuredToBe pathType -> FilePath
unWrap = toAbsolute |> help where help (AssuredToBe _ path) = path

{- |
denotes a type of filepath for which

there is some method to convert 

any filepath of that type to an absolute file path
-}
class Absolutable pathType where
    toAbsolute :: AssuredToBe pathType  -> AbsoluteFilePath

data Absolute = Absolute deriving Show

instance Absolutable Absolute where toAbsolute = id

{- |
Denotes an absolute File path

Type level assurance that the filepath associated with an object of this type is absolute

Use @makeAbsolute@ to create an AbsoluteFilePath object
-}
type AbsoluteFilePath = AssuredToBe Absolute

{- |
Will try its best to transform the input into an absolute file path

If not successful in this task, throws error
-}
makeAbsolute :: FilePath -> IO AbsoluteFilePath
makeAbsolute =
    D.makeAbsolute
    |> fmap ( AssuredToBe Absolute )

newtype RootRelative = RelativeTo { root :: AbsoluteFilePath } deriving Show

{- |
Denotes a file path relative to @root@

Type level assurance that the filepath associated with an object of this type is relative to @root@

Use @relativizeToRoot@ to create a RootRelativeFilePath object
-}
type RootRelativeFilePath = AssuredToBe RootRelative

rootOf :: RootRelativeFilePath -> AbsoluteFilePath
rootOf (AssuredToBe pathType _) = root pathType

{- |
Will try its best to relativize the input to @root@

If not successful in this task, throws error
-}
relativizeToRoot :: AbsoluteFilePath -> IO RootRelativeFilePath
relativizeToRoot (AssuredToBe Absolute path) = let root = findRoot path in
    ( AssuredToBe . RelativeTo .  AssuredToBe Absolute <$> root  ) <*> ( F.makeRelative <$> root <*> pure path )

-- | @\\@ Directory @d ->@ list of ancestor directories of @d@
-- 
-- >>> ancestors "d1\\d2\\d3\\f.x" == ["d1\\","d1\\d2\\","d1\\d2\\d3\\"]
ancestors :: FilePath -> [FilePath]
ancestors =
    F.takeDirectory
    |> F.addTrailingPathSeparator
    |> F.splitPath
    |> scanl' (F.</>) ""
    |> tail

{- | 
Given a target file, return 

root directory of the project

defined as the outermost ancestor directory of target that contains the "hie.yaml" config file.
-}
findRoot :: FilePath -> IO FilePath
findRoot path =
    path
    ||> ancestors
    |> filterM ( D.doesFileExist . ( F.</> "hie.yaml" ) )
    |> fmap (
        listToMaybe
        |> fromMaybe ( error "\nA \"hie.yaml\" file is required to be exist to define a root.\nThe given path "++show path++" is not in the scope of any defined root" )
    )

instance Absolutable RootRelative where
    toAbsolute :: AssuredToBe RootRelative -> AbsoluteFilePath
    toAbsolute ( AssuredToBe ( RelativeTo ( AssuredToBe Absolute root ) ) path ) = AssuredToBe Absolute ( root F.</> path )