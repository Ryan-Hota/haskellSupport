{-# OPTIONS_GHC -Wall #-}

import System.FilePath( (</>), (<.>), takeDirectory, isDrive, pathSeparator, getSearchPath )
import System.Directory (copyFile, getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist, removeFile, exeExtension, removeDirectory, doesDirectoryExist, setCurrentDirectory, withCurrentDirectory, XdgDirectory (XdgData), getXdgDirectory)
import System.Process (CreateProcess(..), createProcess, shell, StdStream (CreatePipe), waitForProcess)
import System.IO(hGetContents', hGetContents)
import Control.Monad (filterM, when, void, unless)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Exit (ExitCode (..))
import Prelude hiding (error)
import qualified Prelude
import Data.Functor ((<&>))

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
    path <- getXdgDirectory XdgData "haskellSupport"
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
        else error "could not complete ghc compilation of haskellSupport"

    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++ show (exeDir</>"haskellSupport"<.>exeExtension) ++"\n    }\n  }")

data To = To

copy :: FilePath -> To -> FilePath -> IO ()
copy cabalExecutable To exeDir = do

    cabalAccessible <-  run "cabal --help"
    unless cabalAccessible $ error "the cabal package manager for haskell cannot be accessed from the \"userSpace\" directory"

    void $ run ("cabal install "++cabalExecutable)

    (_,Just hout,_,_) <- createProcess ((shell "cabal path"){std_out=CreatePipe})
    out <- hGetContents' hout
    let cabalInstallDir = drop 12 . last . lines $ out

    exeExists <- doesFileExist (cabalInstallDir</>cabalExecutable<.>exeExtension)
    if exeExists
        then copyFile (cabalInstallDir</>cabalExecutable<.>exeExtension) (exeDir</>cabalExecutable<.>exeExtension)
        else error ("Could not find "++show (cabalExecutable<.>exeExtension)++", a required haskell package, upon searching for it in "++show cabalInstallDir++", the standard location for haskell packages installed through the cabal haskell package manager")

main :: IO ()
main = do

    maybeUserSpace <- fmap listToMaybe . filterM (doesFileExist.(</>"hie.yaml").takeDirectory) . takeWhile (not.isDrive) . iterate takeDirectory =<< getCurrentDirectory
    let userSpace = ( `fromMaybe` maybeUserSpace ) (error "please ensure that the current working directory is inside the \"userSpace\" folder" )
    setCurrentDirectory userSpace

    exeDir <- appPath <&> (</>"bin")
    createDirectoryIfMissing True exeDir
    (elem exeDir <$> getSearchPath) >>= (`unless` error ("please PERMANENTLY add " ++ show exeDir ++ " to PATH"))

    vscodeAccessible <- run "code --help"
    if vscodeAccessible
        then do
            void $ installVscodeExtension "mogeko.haskell-extension-pack"
            void $ installVscodeExtension "formulahendry.code-runner"
            void $ installVscodeExtension "tomoki1207.pdf"
        else doesDirectoryExist ".vscode" >>= ( `when` removeDirectory ".vscode" )

    compileHaskellSupportAt exeDir

    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++ show (exeDir</>"haskellSupport"<.>exeExtension)++"\n    }\n  }")

    -- _ <- if os=="mingw32" 
        -- then run "powershell -ExecutionPolicy Bypass -File addToPath.ps1"
        -- else run "echo \"remember to add "++show (show exeDir)++" to PATH\""

    copy "hlint" To exeDir
    copy "markdown-unlit" To exeDir

    run "ghcup install hls" >>= ( `unless` error "Could not install haskell-language-server (HLS), the program that communicates between haskell and the text editor" )

    putStrLn "\ESC[32m\n\nhaskellSupport installation complete!\n\n\ESC[0m"

    firstExists <- doesFileExist "helloWorld.hs"
    let runWithoutWaiting = void.createProcess.shell in case ( vscodeAccessible , firstExists ) of
        (True,True) -> run ("code \"."++[pathSeparator]++"\"") >> runWithoutWaiting "code --goto helloWorld.hs:1:1"
        (True,False) -> runWithoutWaiting ("code \"."</>"\"")
        (False,True) -> runWithoutWaiting "haskellSupport \"helloWorld.hs\" loadGHCiInShell"
        (False,False) -> runWithoutWaiting "ghci"