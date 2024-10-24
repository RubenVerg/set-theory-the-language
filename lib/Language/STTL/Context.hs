module Language.STTL.Context
  ( Context
  , runContext
  , liftIO
  , lift
  , throwError
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State

-- | The context monad.
type Context = ExceptT String IO

-- | Evaluate a 'Context' computation to @IO@.
runContext :: Context a -> IO (Either String a)
runContext = runExceptT
