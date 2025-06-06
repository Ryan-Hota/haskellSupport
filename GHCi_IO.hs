module GHCi_IO (
    loadInShell ,
    loadInGHCi
) where

import Options (options)
import Directory_IO (fileTreeAlong, getCurrentDirectory)
import Target_IO (Target, targetPath)
import FilePath (unWrap, makeAbsolute, makeRelativeTo, relativizeToRoot, RootRelativeFilePath)
import System.Process (shell ,createProcess, waitForProcess)
import Utilities ((|>))
import Shadow_IO (withShadowOf)
import Control.Monad (void)
import OS_IO ( clearScreenCommand )
import System.IO (readFile')
import System.Environment (setEnv, getEnv)
import Data.List (partition)
import System.FilePath (pathSeparator)

ghciOptions :: [String] -> ([String],[String])
ghciOptions = 
    filter (take 1|>(/="-"))
    |> partition (take 10|>(==":set -pgmL"))

getUserCmds :: RootRelativeFilePath -> RootRelativeFilePath -> Target -> IO String
getUserCmds currentDir shadowDir target = do
    fileTreeAlongTarget <- fileTreeAlong $ targetPath target
    let opts = ghciOptions ( options fileTreeAlongTarget )
    pure $ unlines (
        [ ":! " ++ clearScreenCommand ]
        ++ [ "::set -i" ++ makeRelativeTo currentDir shadowDir ]
        ++ fst opts
        ++ [ "::load " ++ show ( unWrap $ targetPath target ) ]
        ++ [""]
        ++ snd opts
        )

loadInShell :: Target -> IO ()
loadInShell target = void.waitForProcess.(\(_,_,_,x)->x) =<< do

    currentDir <- relativizeToRoot =<< getCurrentDirectory

    withShadowOf target $ \ shadowDir -> do
        userCmds <- getUserCmds currentDir shadowDir target
        writeFile ".ghci" (
            "import qualified System.Environment as SystemEnvironmentForHaskellSupport\n"
            ++ "import qualified Control.Applicative as ControlApplicativeForHaskellSupport\n"
            ++ "::def! load (\\path->let pure = ControlApplicativeForHaskellSupport.pure ; (<|>) = (ControlApplicativeForHaskellSupport.<|>) in pure ( \":! haskellSupport  \" <|> path <|> \" loadGHCiInGHCi\" <|> \"\\n:reload\" ) )\n"
            ++ "::def! l (\\path->let pure = ControlApplicativeForHaskellSupport.pure ; (<|>) = (ControlApplicativeForHaskellSupport.<|>) in pure ( \":load \" <|> path ) )\n"
            ++ "::def! reload (\\_->do{script<-SystemEnvironmentForHaskellSupport.getEnv \"HASKELLSUPPORT_GHCI_SCRIPT\" ; ControlApplicativeForHaskellSupport.pure (\"::script \" ControlApplicativeForHaskellSupport.<|> script)})\n"
            ++ "::def! r (\\_-> ControlApplicativeForHaskellSupport.pure \":reload\")\n"
            ++ userCmds
            )
        setEnv "HASKELLSUPPORT_GHCI_SCRIPT" . unWrap =<< makeAbsolute ".ghci"

    scriptFilePath <- getEnv "HASKELLSUPPORT_GHCI_SCRIPT"
    (createProcess.shell) ( "ghci -ghci-script " ++ show scriptFilePath )

loadInGHCi :: Target -> IO ()
loadInGHCi target = void $ do

    userCmdsBefore <- readFile' =<< getEnv "HASKELLSUPPORT_GHCI_SCRIPT"

    currentDir <- relativizeToRoot =<< getCurrentDirectory

    withShadowOf target $ \ shadowDir -> do
        userCmds <- getUserCmds currentDir shadowDir target
        writeFile ".ghci" $ if dropWhile (not.null) ( lines userCmdsBefore ) == dropWhile (not.null) ( lines userCmds )
            then userCmds
            else ":! haskellSupport " ++ show ( unWrap $ targetPath target ) ++ " loadGHCiInShell" ++ "\n::quit\n"
        setEnv "HASKELLSUPPORT_GHCI_SCRIPT" . unWrap =<< makeAbsolute ".ghci"

    -- >>> :reload
