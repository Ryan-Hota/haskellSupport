import System.Environment (getArgs)
import System.IO (readFile')
import Target_IO (targetPath, takeAsTarget)
import FilePath (makeAbsolute)
import qualified Bios_IO
import qualified GHCi_IO
import qualified Options
import qualified Modules
import Directory_IO (fileTreeAlong)
import Control.Monad ((<=<))
import Directory (path)
import qualified Shadow_IO
import Utilities ((||>))

main :: IO ()
main = do

    args <- getArgs
    -- print args

    case args of


        [] ->

            let hieConfig = "hie.yaml" in
                readFile' hieConfig
                >>= writeFile hieConfig


        ( targetString : otherArgs ) ->

                ( targetString ||> makeAbsolute ) 
                >>= takeAsTarget

                >>= case otherArgs of

                        [] -> Bios_IO.main

                        ["loadGHCiInShell"] -> GHCi_IO.loadInShell

                        ["loadGHCiInGHCi"] -> GHCi_IO.loadInGHCi

                        ["options"] -> mapM_ putStrLn . Options.options <=< fileTreeAlong . targetPath

                        ["modules"] -> mapM_ (print.path) . Modules.modules <=< fileTreeAlong . targetPath

                        ["bios"] -> Bios_IO.test

                        ["clean"] -> (>>) <$> Bios_IO.clean <*> Shadow_IO.clean

                        [unknownCmd] -> const $ putStrLn ( "command "++show unknownCmd++" unknown. Known commands are loadGHCiInShell, loadGHCiInGHCi, options, modules, bios, and clean." )

                        _ -> const $ putStrLn "too many commands: max arity is 2"
