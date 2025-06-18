{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

import System.FilePath( (</>), (<.>), takeDirectory, isDrive, pathSeparator, getSearchPath, takeBaseName )
import System.Directory (copyFile, getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist, exeExtension, doesDirectoryExist, setCurrentDirectory, withCurrentDirectory, XdgDirectory (XdgData), getXdgDirectory, getPermissions, Permissions (writable))
import System.Process (CreateProcess(..), createProcess, shell, StdStream (CreatePipe), waitForProcess)
import System.IO(hGetContents', hGetContents)
import Control.Monad (filterM, when, void, unless)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Exit (ExitCode (..))
import Prelude hiding (error)
import qualified Prelude
import Data.Functor ((<&>))
import System.Environment (setEnv, lookupEnv, getEnv)
import Data.Char (isSpace)

highlight :: [Int] -> String ->  String
highlight l str = (f <$>id<*>reverse) s where
    f x y = "\n\ESC[6m" ++ x ++ "\ESC[0m" ++ concat [ "\ESC[" ++ show n ++ "m" | n <- l ] ++ "\n\n" ++ str ++ "\n\n\ESC[0m" ++ "\ESC[6m" ++ y ++ "\ESC[0m"
    s = "*\n**\n****\n********\n****************\n********************************\n****************************************************************"

error :: String -> any
error = Prelude.error . highlight [91] . (++"\n\nPlease try to resolve the above error and run again.\nIf you are unsure about how to do this, or have faced and tried to resolve this error a few times already, please contact a teaching assistant.")

instruction :: String -> any
instruction = Prelude.error . highlight [95] . (++"\n\nPlease try to follow the above instruction(s) and run again.\nIf you are unsure about how to do this, or have seen and tried to follow the instruction(s) a few times already, please contact a teaching assistant.")

run :: String -> IO Bool
run str = do
    putStrLn ( "\ESC[96mRUNNING COMMAND - " ++ str ++ "\ESC[0m")
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
main ={- (<*) (putStrLn "hello") $ const (pure ()) $ -}do

    maybeHaskell <- fmap listToMaybe . filterM (doesFileExist.(</>"rootOfHaskellSupport").takeDirectory) . takeWhile (not.isDrive) . iterate takeDirectory =<< getCurrentDirectory
    let haskell = ( `fromMaybe` maybeHaskell ) (instruction "Please ensure that the current working directory is inside the \"haskell\" folder" )
    setCurrentDirectory haskell

    vscodeExists <- run "code --help"
    when vscodeExists $ do
        void $ installVscodeExtension "haskell.haskell"
        void $ installVscodeExtension "formulahendry.code-runner"
        void $ installVscodeExtension "tomoki1207.pdf"

    exeDir <- appPath <&> (</>"bin")
    createDirectoryIfMissing True exeDir

    run "cabal --help" >>= (`unless` instruction "The \"cabal\" build-tool for haskell cannot be accessed from within the \"haskell\" directory.\nIt is either not in PATH or not yet installed.\nIt was supposed to be automatically installed along with the installing of haskell.\n\nMaybe you should install haskell once more according to the given instructions, or if you know that you have the cabal executable on your device, you should add its parent folder to PATH.")
    installCabalPackage "hlint"

    addToPATH exeDir

    (_,Just hout,_,_) <- createProcess ((shell "ghc --numeric-version"){std_out=CreatePipe})
    ghcVersion <- init <$> hGetContents' hout
    let hlsName = "haskell-language-server-" ++ ghcVersion
    run (hlsName++" --help") >>= (`unless` instruction ("The required version of the haskell language server protocol, namely "++show hlsName++" cannot be accessed from within the \"haskell\" directory.\nIt is either not in PATH or not yet installed.\nIt was supposed to be automatically installed along with the installing of haskell.\n\nMaybe you should install haskell once more according to the given instructions, or if you know that you have the "++hlsName++" executable on your device, you should add its parent folder to PATH." ))

    compileHaskellSupportAt exeDir
    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++ show (exeDir</>"haskellSupport"<.>exeExtension)++"\n    }\n  }")

    putStrLn "\ESC[92m\n\nhaskellSupport installation complete!\n\n\n\n\ESC[0m\n"

    helloWorldFileExists <- doesFileExist "helloWorld.hs"
    let runWithoutWaiting = void.createProcess.shell in case ( vscodeExists , helloWorldFileExists ) of
        (True,True) -> runWithoutWaiting ("code \"."++[pathSeparator]++"\"") >> runWithoutWaiting "code --goto helloWorld.hs:1:1"
        (True,False) -> runWithoutWaiting ("code \"."</>"\"")
        (False,True) -> runWithoutWaiting "haskellSupport \"helloWorld.hs\" loadGHCiInShell"
        (False,False) -> runWithoutWaiting "ghci"

installVscodeExtension :: String -> IO Bool
installVscodeExtension extId = run ("code --install-extension "++extId)

installCabalPackage :: String -> IO ()
installCabalPackage cabalExecutable = do

    void $ run ("cabal install "++cabalExecutable)

    (_,Just hout,_,_) <- createProcess ((shell "cabal path"){std_out=CreatePipe})
    out <- hGetContents' hout
    let cabalInstallDirParser = \case {
        '\n':'i':'n':'s':'t':'a':'l':'l':'d':'i':'r':':':rest -> dropWhile isSpace . head . lines $ rest ;
        _ : cs -> cabalInstallDirParser cs ;
        "" -> error "Unknown cabal path syntax, contact a teaching assistant"
    }
    let cabalInstallDir = cabalInstallDirParser out

    exeExists <- doesFileExist (cabalInstallDir</>cabalExecutable<.>exeExtension)
    unless exeExists $ error ("Could not find "++show (cabalExecutable<.>exeExtension)++", a required haskell package, upon searching for it in "++show cabalInstallDir++", the standard location for haskell packages installed through the \"cabal\"build-tool for haskell")

compileHaskellSupportAt :: FilePath -> IO ()
compileHaskellSupportAt path = do

    tmpDir <- appPath <&> (</>"temporary")
    createDirectoryIfMissing True tmpDir

    root <- takeDirectory <$> getCurrentDirectory
    let localExeDir = root</>"executable"
    listDirectory localExeDir >>= mapM_ (copyFile<$>(localExeDir</>)<*>(tmpDir</>))

    compilationSucceeded <- withCurrentDirectory tmpDir $ run ( "ghc -i"++show tmpDir++" -O2 "++show (tmpDir</>"haskellSupport.hs") )
    if compilationSucceeded
        then copyFile (tmpDir</>"haskellSupport"<.>exeExtension) (path</>"haskellSupport"<.>exeExtension)
        else error "Could not complete ghc compilation of haskellSupport"

addToPATH :: FilePath -> IO ()
addToPATH path = (>>=) (elem path <$> getSearchPath) . flip unless $ (do
    attempt1 <-
        setEnv "HASKELLSUPPORT_EXECUTABLES_DIRECTORY_PATH" path
        >> run ("powershell -ExecutionPolicy ByPass -File "++".."</>"init"</>"addToPATH.ps1")
        >>= ( \ psScriptRan -> ( psScriptRan && ) . (/=Just "failed") <$> lookupEnv "ADD_HASKELLSUPPORT_TO_PATH_FAILURE")
    setEnv "ADD_HASKELLSUPPORT_TO_PATH_FAILURE" "reset"
    attempt2 <- addToShellConfig path
    unless ( attempt1 || attempt2 ) (instruction ("Please PERMANENTLY add \n\n" ++ path ++ "\n\n to your PATH.\nRemember: You will need to open a new session since the changes to PATH will not be visible in this session.\n\nIf you really really don't want to add it to PATH, you can add it temporarily and run again, but please remember to do the same in all future haskell-running terminal sessions.") ))

data Shell = Bash | ZShell | Fish deriving Show

profileFile :: Shell -> IO FilePath
profileFile shell_ =
    case shell_ of
        Bash   -> getEnv "HOME" <&> ( </> ".bashrc" )
        ZShell ->
            lookupEnv "ZDOTDIR"
            >>= ( \case
                Just zDotDir -> pure zDotDir
                Nothing      -> getEnv "HOME"
            ) <&> ( </> ".zshrc" )
        Fish   -> getEnv "HOME" <&> ( </> ".config"</>"fish"</>"config.fish" )

findShell :: IO (Maybe Shell)
findShell = do
    maybeShellPath <- lookupEnv "SHELL"
    case takeBaseName <$> maybeShellPath of
        Just "zsh"  -> pure $ Just ZShell
        Just "bash" -> pure $ Just Bash
        Just "sh"   ->
            mapM lookupEnv ["BASH","ZSH_VERSION"]
            <&> ( \case
                (Just _:_       ) -> Just Bash
                (     _:Just _:_) -> Just ZShell
                _                 -> Nothing
            )
        Just "fish" -> pure $ Just Fish
        _           -> pure Nothing

expressionToAdd :: [Char] -> Shell -> [Char]
expressionToAdd path = \case
    Bash   -> "\n# added by haskellSupport\ncase \":${PATH}:\" in\n*:\""++path++"\":*) ;;\n*)\n    export PATH=\"$PATH:"++path++"\"\n    ;;\nesac\n"
    ZShell -> "\n# added by haskellSupport\ncase \":${PATH}:\" in\n*:\""++path++"\":*) ;;\n*)\n    export PATH=\"$PATH:"++path++"\"\n    ;;\nesac\n"
    Fish   -> "\n# added by haskellSupport\nfish_add_path \""++path++"\"\n"

addToShellConfig :: FilePath -> IO Bool
addToShellConfig path = (>>=) findShell . (\action-> \case {Just shell_->action shell_;Nothing->pure False}) $ \ shell_ ->  do
    shellConfigPath <- profileFile shell_
    homeDir <- getEnv "HOME"
    withCurrentDirectory homeDir $ do
        shellConfigParentExists <- doesDirectoryExist $ takeDirectory shellConfigPath
        unless shellConfigParentExists $ instruction ("The parent directory of your "++show shell_++" shell configuration file / shell profile file , namely "++show (takeDirectory shellConfigPath) ++ ", does not exist.\nPlease create it.")
        shellConfigPathExists <- doesFileExist shellConfigPath
        unless shellConfigPathExists $ writeFile shellConfigPath ""
        shellConfigPathWritable <- writable <$> getPermissions shellConfigPath
        when shellConfigPathWritable . appendFile shellConfigPath $ expressionToAdd path shell_
        pure shellConfigPathWritable