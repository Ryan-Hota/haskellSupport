module Directory_IO (
    fileTreeAlong,
    doesFileExist,
    withCurrentDirectory,
    listPermittedDirectory,
    mkLinkAt,
    fileTreeUnder,
    getCurrentDirectory,
    removePathForcibly
) where

import FilePath
    ( (</>),
      splitPath,
      unWrap,
      AssuredToBe,
      Absolutable(..),
      takeName,
      AbsoluteFilePath,
      makeAbsolute )

import Utilities ((|>), ifThenElse, (||>))
import System.IO.Unsafe (unsafeInterleaveIO)
import Directory (FileTree, FileTree (..))
import System.FilePath (equalFilePath)
import qualified System.Directory as D
import IO (readPermittedFile)
import OS_IO (mkHardLink)
import System.Directory (doesPathExist)

removePathForcibly :: Absolutable pathType => AssuredToBe pathType -> IO ()
removePathForcibly = unWrap |> D.removePathForcibly

-- | @\\ dir file ->@\\
-- make a  __/hard/__  link located in @dir@ that points to the @file@\\
-- and\\
-- return the name of the created link
mkLinkAt :: (Absolutable pathType0, Absolutable pathType1) => AssuredToBe pathType0 -> AssuredToBe pathType1 -> IO String
mkLinkAt dir file =
    doesPathExist ( unWrap (dir</>name) )
    >>= \ b -> if b
        then 
            removePathForcibly (dir</>name)
            >> mkLinkAt dir file
        else 
            mkHardLink (unWrap file) (unWrap (dir</>name))
            >> pure name
    where name = takeName file

doesFileExist :: Absolutable pathType => AssuredToBe pathType -> IO Bool
doesFileExist = unWrap |> D.doesFileExist

getCurrentDirectory :: IO AbsoluteFilePath
getCurrentDirectory = makeAbsolute =<< D.getCurrentDirectory

withCurrentDirectory :: Absolutable pathType => AssuredToBe pathType -> IO b -> IO b
withCurrentDirectory dirPath action = let unWrappedDirPath = unWrap dirPath in
    D.createDirectoryIfMissing True unWrappedDirPath
    >> D.withCurrentDirectory unWrappedDirPath action

listPermittedDirectory :: Absolutable pathType => AssuredToBe pathType -> IO [String]
listPermittedDirectory path = -- pure $ error "check lazy"
    ifThenElse
        <$> ( path ||> unWrap |> D.getPermissions |> fmap D.searchable )
        <*> ( path ||> unWrap |> D.listDirectory )
        <*> pure []

-- | Given /target/, get the file tree that surrounds the path to /target/
fileTreeAlong :: Absolutable pathType => AssuredToBe pathType -> IO (FileTree (AssuredToBe pathType))
fileTreeAlong =
    splitPath
    |> ( \ ( topDir , descendants ) -> fileTreeAlongNamed ( takeName topDir ) topDir descendants )

fileTreeAlongNamed :: Absolutable pathType => String -> AssuredToBe pathType -> [String] -> IO (FileTree (AssuredToBe pathType))
fileTreeAlongNamed name_ path (child:childDescendants) =
    ( listDir <$> fileTreeUnderNamed name_ path )
    ||> fmap ( filter (name|>(not.equalFilePath child)) )
    ||> unsafeInterleaveIO
    ||> ( (:) <$> fileTreeAlongNamed child ( path </> child ) childDescendants <*> )
    ||> fmap ( Directory name_ path )
fileTreeAlongNamed name path [] = fileTreeUnderNamed name path

fileTreeUnder :: Absolutable pathType => AssuredToBe pathType -> IO (FileTree (AssuredToBe pathType))
fileTreeUnder = fileTreeUnderNamed <$> takeName <*> id

fileTreeUnderNamed :: Absolutable pathType => String -> AssuredToBe pathType -> IO (FileTree (AssuredToBe pathType))
fileTreeUnderNamed name path =
    ifThenElse
        <$> doesFileExist path
        <*> (
            readPermittedFile path
            ||> unsafeInterleaveIO
            ||> fmap ( File name path )
        )
        <*> (
            listPermittedDirectory path
            >>= mapM ( fileTreeUnderNamed <$> id <*> ( path </> ) )
            ||> unsafeInterleaveIO
            ||> fmap ( Directory name path  )
        )
    ||> unsafeInterleaveIO
