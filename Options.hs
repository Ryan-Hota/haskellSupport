{-# OPTIONS_GHC -Wno-missing-fields #-}
module Options (
    options
) where

import Utilities ((|>), (||>))
import FilePath (RootRelativeFilePath)
import Directory (FileTree(..), isFile, isDirectory)

-- | characters to ignore if found in the /options.txt/ file
charsToIgnore :: String
charsToIgnore = ""--"[]{}(),;:"

-- | format a text string containing all options into a list of options
format :: String -> [String]
format =
    map ( \ c -> if c `elem` charsToIgnore then ' ' else c )
    |> lines --words

-- | list of options
options :: FileTree RootRelativeFilePath -> [String]
options tree =
    tree
    ||> iterate (listDir|>head)
    |> takeWhile isDirectory
    |> concatMap listDir
    |> filter ((&&)<$>isFile<*>name|>(=="options.txt"))
    |> map (format.contents)
    |> concat
