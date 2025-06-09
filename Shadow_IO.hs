{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Shadow_IO (
    withShadowOf,
    clean
) where

import FilePath (makeAbsolute, AbsoluteFilePath, unWrap, (</>), rootOf, RootRelativeFilePath)
import Target_IO (targetPath, Target)
import Utilities ((|>), (||>))
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Directory_IO ( withCurrentDirectory, removePathForcibly,fileTreeAlong, mkLinkAt)
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath (takeDrive, isPathSeparator)
import Control.Monad ((>=>), forM_, void)
import Modules(modules)
import Directory(path)
import System.Info(os)

import qualified FilePath_Internal_IO as FI
rootOfShadowOf :: Target -> RootRelativeFilePath
rootOfShadowOf target = help $ targetPath target where
    help (FI.AssuredToBe pathType _) = FI.AssuredToBe pathType "preShadowRoot" </> "shadowRoot"

shadowOf :: Target -> RootRelativeFilePath
shadowOf target =
    target
    ||> targetPath
    |> unWrap
    |> concatMap replace
    |> ( rootOfShadowOf target </> )
    where
        replace               char
            | isPathSeparator char = "_SEP_"
            | isSpace         char = "_SPACE_"
            | ':' ==          char = "_COLON_"
            | '.' ==          char = "_DOT_"
            | otherwise            = [char]

withShadowOf :: Target -> ( RootRelativeFilePath -> IO a ) -> IO ()
withShadowOf target action = void $ do
    let shadowDir = shadowOf target
    createDirectoryIfMissing True $ unWrap shadowDir
    withCurrentDirectory shadowDir ( do
        fileTreeAlongTarget <- fileTreeAlong $ targetPath target
        forM_ ( modules fileTreeAlongTarget ) ( path |> mkLinkAt shadowDir )
        action shadowDir
        )

clean :: Target -> IO ()
clean = rootOfShadowOf |> removePathForcibly