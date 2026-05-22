module Command.Common where

import Data.IORef (IORef)
import Control.Monad.Trans.Reader (ReaderT)

data AppState = AppState 


newtype Env = Env {
  appState :: AppState
}

type App a = ReaderT Env IO a