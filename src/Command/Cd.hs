module Command.Cd where

import Data.IORef (writeIORef)
import Command.Common (AppState(..), Env (..), App)
import System.Directory (doesDirectoryExist)
import Control.Monad.Reader (lift, ask)

cdCommand :: [String] -> App ()
cdCommand args = do
    env <- ask
    case args of
        (path : _) -> lift $ do
            let dirRef = currentWorkingDirectory . appState $ env
            dirExist <- doesDirectoryExist path
            if dirExist then
                writeIORef dirRef path
            else
                putStrLn $ "cd: " ++ path ++ ": No such file or directory"
        _ -> return ()