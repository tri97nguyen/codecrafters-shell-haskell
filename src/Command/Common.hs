module Command.Common where

import Data.IORef (IORef)
import Control.Monad.Trans.Reader (ReaderT)

newtype AppState = AppState {
  currentWorkingDirectory :: IORef FilePath
}

newtype Env = Env {
  appState :: AppState
}

type App a = ReaderT Env IO a