{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

import System.FilePath( (</>), (<.>), takeDirectory, isDrive, pathSeparator, getSearchPath, takeDrive )
import System.Directory (copyFile, getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist, removeFile, exeExtension, removeDirectory, doesDirectoryExist, setCurrentDirectory, withCurrentDirectory, XdgDirectory (XdgData), getXdgDirectory, getHomeDirectory, getAppUserDataDirectory)
import System.Process (CreateProcess(..), createProcess, shell, StdStream (CreatePipe), waitForProcess)
import System.IO(hGetContents', hGetContents)
import Control.Monad (filterM, when, void, unless)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Exit (ExitCode (..))
import Prelude hiding (error)
import qualified Prelude
import Data.Functor ((<&>))
import System.Info (os)
import Data.Char (isSpace)

error :: String -> any
error = Prelude.error . ("\ESC[91m"++) . (++"\ESC[0m")

-- warning :: String -> IO Bool
-- warning = (True <$) . putStrLn . ("\ESC[33m"++) . (++"\ESC[0m")

run :: String -> IO Bool
run str = do
    putStrLn ( "\ESC[32mRUNNING COMMAND - " ++ str ++ "\ESC[0m")
    (_,Just hout,Just herr,ph) <- createProcess ((shell str){std_out=CreatePipe,std_err=CreatePipe})
    putStrLn =<< hGetContents hout
    putStrLn =<< hGetContents herr
    exitCode <- waitForProcess ph
    pure $ case exitCode of
        ExitSuccess -> True
        ExitFailure _ -> False

installVscodeExtension :: String -> IO Bool
installVscodeExtension extId = run ("code --install-extension "++extId)

appPath :: IO FilePath
appPath = do
    -- tentative <- case os of 
    --     "mingw32" -> ( takeDrive <$> getHomeDirectory ) <&> (</>"haskellSupport")
    --     "linux" -> getXdgDirectory XdgData "haskellSupport"
    --     unknown -> error ("your operating system "++show unknown++" is unsupported")
    -- when (any isSpace tentative) $ error ("path "++show tentative++" to the app has whitespaces")
    -- let path = tentative
    path <- getAppUserDataDirectory "haskellSupport"
    createDirectoryIfMissing True path
    pure path

compileHaskellSupportAt :: FilePath -> IO ()
compileHaskellSupportAt exeDir = do

    root <- takeDirectory <$> getCurrentDirectory

    let localExeDir = root</>"executable"
    listDirectory localExeDir >>= mapM_ (copyFile<$>(localExeDir</>)<*>(exeDir</>))

    compilationSucceeded <- withCurrentDirectory exeDir $ run ( "ghc -i"++show exeDir++" -O2 "++show (exeDir</>"haskellSupport.hs") )
    if compilationSucceeded
        then mapM_ (removeFile.(exeDir</>)) . filter (/=("haskellSupport"<.>exeExtension)) =<< listDirectory exeDir
        else error "ghc compilation of haskellSupport failed"

    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++ show (exeDir</>"haskellSupport"<.>exeExtension) ++"\n    }\n  }")

data To = To

copy :: FilePath -> To -> FilePath -> IO ()
copy cabalExecutable To exeDir = do

    cabalAccessible <-  run "cabal --help"
    unless cabalAccessible $ error "cabal cannot be accessed here"

    void $ run ("cabal install "++cabalExecutable)

    (_,Just hout,_,_) <- createProcess ((shell "cabal path"){std_out=CreatePipe})
    out <- hGetContents' hout
    let cabalInstallDir = drop 12 . last . lines $ out

    exeExists <- doesFileExist (cabalInstallDir</>cabalExecutable<.>exeExtension)
    if exeExists
        then copyFile (cabalInstallDir</>cabalExecutable<.>exeExtension) (exeDir</>cabalExecutable<.>exeExtension)
        else error ("Search for "++show (cabalExecutable<.>exeExtension)++" in "++show cabalInstallDir++" failed.")

main :: IO ()
main = do

    maybeUserSpace <- fmap listToMaybe . filterM (doesFileExist.(</>"hie.yaml").takeDirectory) . takeWhile (not.isDrive) . iterate takeDirectory =<< getCurrentDirectory
    let userSpace = ( `fromMaybe` maybeUserSpace ) (error "Could not find user folder (sibling of \"hie.yaml\") among the ancestor folders of the current working folder" )
    setCurrentDirectory userSpace

    exeDir <- appPath <&> (</>"bin")
    createDirectoryIfMissing True exeDir
    (elem exeDir <$> getSearchPath) >>= (`unless` error (show exeDir++"has not been added to PATH"))

    vscodeAccessible <- run "code --help"
    if vscodeAccessible
        then do
            void $ installVscodeExtension "mogeko.haskell-extension-pack"
            void $ installVscodeExtension "formulahendry.code-runner"
            void $ installVscodeExtension "tomoki1207.pdf"
        else doesDirectoryExist ".vscode" >>= ( `when` removeDirectory ".vscode" )

    compileHaskellSupportAt exeDir

    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++(exeDir</>"haskellSupport"<.>exeExtension)++"\n    }\n  }")

    -- _ <- if os=="mingw32" 
        -- then run "powershell -ExecutionPolicy Bypass -File addToPath.ps1"
        -- else run "echo \"remember to add "++show (show exeDir)++" to PATH\""

    copy "hlint" To exeDir
    copy "markdown-unlit" To exeDir

    run "ghcup install hls" >>= ( `unless` error "haskell-language-server (HLS) installation failed" )

    putStrLn "\ESC[32m\n\nhaskellSupport installation complete!\n\n\ESC[0m"

    firstExists <- doesFileExist "first.hs"
    let runWithoutWaiting = void.createProcess.shell in case ( vscodeAccessible , firstExists ) of
        (True,True) -> run ("code \"."++[pathSeparator]++"\"") >> runWithoutWaiting "code --goto first.hs:1:1"
        (True,False) -> runWithoutWaiting ("code \"."</>"\"")
        (False,True) -> runWithoutWaiting "haskellSupport \"first.hs\" loadGHCiInShell"
        (False,False) -> runWithoutWaiting "ghci"