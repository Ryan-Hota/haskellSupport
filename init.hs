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
import System.Environment (setEnv)
import System.Info (os)

highlight :: [Int] -> String ->  String
highlight l str = (f <$>id<*>reverse) s where
    f x y = "\n\ESC[6m" ++ x ++ "\ESC[0m" ++ concat [ "\ESC[" ++ show n ++ "m" | n <- l ] ++ "\n\n" ++ str ++ "\n\n\ESC[0m" ++ "\ESC[6m" ++ y ++ "\ESC[0m"
    s = "*\n**\n****\n********\n****************\n********************************\n****************************************************************" 

error :: String -> any
error = Prelude.error . highlight [91] . (++"\n\nPlease try to resolve the above error and run again.\nIf you are unsure about how to do this, or have faced and tried to resolve this error a few times already, please contact support.")

instruction :: String -> any
instruction = Prelude.error . highlight [96] . (++"\n\nPlease try to follow the above instruction(s) and run again.\nIf you are unsure about how to do this, or have seen and tried to follow the instruction(s) a few times already, please contact support.")

run :: String -> IO Bool
run str = do
    putStrLn ( "\ESC[92mRUNNING COMMAND - " ++ str ++ "\ESC[0m")
    (_,Just hout,Just herr,ph) <- createProcess ((shell str){std_out=CreatePipe,std_err=CreatePipe})
    putStrLn =<< hGetContents hout
    putStrLn =<< hGetContents herr
    exitCode <- waitForProcess ph
    pure $ case exitCode of
        ExitSuccess -> True
        ExitFailure _ -> False

appPath :: IO FilePath
appPath = do
    path <- getXdgDirectory XdgData "haskellSupport"
    createDirectoryIfMissing True path
    pure path

main :: IO ()
main = do

    maybeUserSpace <- fmap listToMaybe . filterM (doesFileExist.(</>"hie.yaml").takeDirectory) . takeWhile (not.isDrive) . iterate takeDirectory =<< getCurrentDirectory
    let userSpace = ( `fromMaybe` maybeUserSpace ) (instruction "Please ensure that the current working directory is inside the \"userSpace\" folder" )
    setCurrentDirectory userSpace

    vscodeExists <- run "code --help"
    if vscodeExists
        then do
            void $ installVscodeExtension "mogeko.haskell-extension-pack"
            void $ installVscodeExtension "formulahendry.code-runner"
            void $ installVscodeExtension "tomoki1207.pdf"
        else doesDirectoryExist ".vscode" >>= ( `when` removeDirectory ".vscode" )

    exeDir <- appPath <&> (</>"bin")
    createDirectoryIfMissing True exeDir

    installCabalPackage "hlint" AndCopyItTo exeDir
    installCabalPackage "markdown-unlit" AndCopyItTo exeDir

    compileHaskellSupportAt exeDir

    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++ show (exeDir</>"haskellSupport"<.>exeExtension)++"\n    }\n  }")

    addToPATH exeDir
    (elem exeDir <$> getSearchPath) >>= (`unless` instruction ("Please PERMANENTLY add \n\n" ++ exeDir ++ "\n\n to your PATH.\n\nIf you really don't want to add it to PATH, you can add it temporarily and run again, but please remember to do the same in all future haskell-running terminal sessions."))

    run "ghcup install hls" >>= ( `unless` error "Could not install haskell-language-server (HLS), the program that enables communication between haskell and the text editor.\nIf you are sure that you already have HLS, please ignore this error, and for your purposes, installation of haskellSupport is complete!" )

    putStrLn "\ESC[92m\n\nhaskellSupport installation complete!\n\n\ESC[0m"

    helloWorldFileExists <- doesFileExist "helloWorld.hs"
    let runWithoutWaiting = void.createProcess.shell in case ( vscodeExists , helloWorldFileExists ) of
        (True,True) -> void $ run ("code \"."++[pathSeparator]++"\" --goto helloWorld.hs:1:1")
        (True,False) -> runWithoutWaiting ("code \"."</>"\"")
        (False,True) -> runWithoutWaiting "haskellSupport \"helloWorld.hs\" loadGHCiInShell"
        (False,False) -> runWithoutWaiting "ghci"

installVscodeExtension :: String -> IO Bool
installVscodeExtension extId = run ("code --install-extension "++extId)

data FillerWord = AndCopyItTo

installCabalPackage :: FilePath -> FillerWord -> FilePath -> IO ()
installCabalPackage cabalExecutable AndCopyItTo exeDir = do

    cabalAccessible <-  run "cabal --help"
    unless cabalAccessible $ error "The \"cabal\" package manager for haskell cannot be accessed from the \"userSpace\" directory"

    void $ run ("cabal install "++cabalExecutable)

    (_,Just hout,_,_) <- createProcess ((shell "cabal path"){std_out=CreatePipe})
    out <- hGetContents' hout
    let cabalInstallDir = drop 12 . last . lines $ out

    exeExists <- doesFileExist (cabalInstallDir</>cabalExecutable<.>exeExtension)
    if exeExists
        then copyFile (cabalInstallDir</>cabalExecutable<.>exeExtension) (exeDir</>cabalExecutable<.>exeExtension)
        else error ("Could not find "++show (cabalExecutable<.>exeExtension)++", a required haskell package, upon searching for it in "++show cabalInstallDir++", the standard location for haskell packages installed through the \"cabal\" haskell package manager")

compileHaskellSupportAt :: FilePath -> IO ()
compileHaskellSupportAt exeDir = do

    tmpDir <- appPath <&> (</>"temporary")
    createDirectoryIfMissing True tmpDir

    root <- takeDirectory <$> getCurrentDirectory
    let localExeDir = root</>"executable"
    listDirectory localExeDir >>= mapM_ (copyFile<$>(localExeDir</>)<*>(tmpDir</>))

    compilationSucceeded <- withCurrentDirectory exeDir $ run ( "ghc -i"++show tmpDir++" -O2 "++show (tmpDir</>"haskellSupport.hs") )
    if compilationSucceeded
        then copyFile (tmpDir</>"haskellSupport"<.>exeExtension) (exeDir</>"haskellSupport"<.>exeExtension)
        else error "Could not complete ghc compilation of haskellSupport"

addToPATH :: String -> IO ()
addToPATH exeDir = do
    setEnv "HASKELLSUPPORT_EXECUTABLES_DIRECTORY_PATH" exeDir
    case os of {
        "mingw32" -> void $ run "powershell -ExecutionPolicy ByPass -File ..\\init\\addToPATH.ps1" ;
        _ -> putStrLn "automated addition of haskellSupport to PATH is still unsupported for your operating system\n"
        }