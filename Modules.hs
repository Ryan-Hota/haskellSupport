{-# OPTIONS_GHC -Wno-missing-fields #-}
module Modules (
    modules , globalModules , localModules, isModule, dotsToSeps
) where
import Directory (FileTree(..), isFile, name, isDirectory)
import Data.Char (isUpper)
import Utilities ((|>), fromMaybe)
import System.FilePath (takeExtension, pathSeparator, takeDirectory)
import FilePath (RootRelativeFilePath)
import Data.List (find, transpose)
import Nub (nubOrdOn)

dotsToSeps :: String -> FilePath
dotsToSeps str = case str of
    ".hs" -> ".hs"
    ".lhs" -> ".lhs"
    '.':cs -> pathSeparator : dotsToSeps cs
    c:cs -> c : dotsToSeps cs
    "" -> error "non-haskell module \""++str ++ "\" was given to dotsToSeps"

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

-- | is a file, name begins with a capital letter, has extension ".hs" or ".lhs"
isModule :: FileTree t -> Bool
isModule =
    isFile
    <&&> (
        name |> (
            (not.null)
            <&&> (head|>isUpper)
            <&&> (takeExtension|>(`elem`[".hs",".lhs"]))
        )
    )

-- | all modules (with repeated names) in the ancestor directories of target
localModules :: FileTree RootRelativeFilePath -> [FileTree RootRelativeFilePath]
localModules =
    iterate (listDir|>head)
    |> takeWhile isDirectory
    |> reverse
    |> concatMap listDir
    |> filter isModule

-- | all modules (with repeated names) in the modules directory
-- globalModules :: IO [FilePath]
globalModules :: FileTree RootRelativeFilePath -> [FileTree RootRelativeFilePath]
globalModules =
    listDir
    |> find ((&&)<$>isDirectory<*>name|>(=="globallyEnabledModules"))
    |> fromMaybe defaultGlobalModulesDirectory
    |> breadthFirstSearch
    |> filter isModule
    where
        defaultGlobalModulesDirectory = 
            Directory {
                name = "globallyEnabledModules",
                listDir = []
            }
        levels x = [x] : if isFile x then [] else concat <$> transpose $ levels <$> listDir x
        breadthFirstSearch = concat . levels

-- | all modules, with repeated names, \\
-- (those in the ancestor directories of target) and (those in the Modules Folder), \\
-- in that order
allModules :: FileTree RootRelativeFilePath -> [FileTree RootRelativeFilePath]
allModules = (++) <$> localModules <*> globalModules

-- | all modules which target might potentially import,\\
-- and in the case there are multiple files with the same name,\\
-- take the one which is closer to target 
modules :: FileTree RootRelativeFilePath -> [FileTree RootRelativeFilePath]
modules = allModules |> nubOrdOn name