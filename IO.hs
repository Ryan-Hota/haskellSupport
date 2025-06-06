module IO (
    readPermittedFile
) where

import FilePath (Absolutable (..), AssuredToBe, unWrap)
import Utilities (ifThenElse, (||>), (|>))
import System.Directory (getPermissions, Permissions (..))
-- import qualified System.IO as I

readPermittedFile :: Absolutable pathType => AssuredToBe pathType -> IO String
readPermittedFile path =
    ifThenElse
        <$> ( path ||> unWrap |> getPermissions |> fmap readable )
        <*> ( path ||> unWrap |> readFile )
        <*> pure "read permission denied"

-- readFile' :: Absolutable pathType => AssuredToBe pathType -> IO String
-- readFile' = unWrap |> I.readFile'