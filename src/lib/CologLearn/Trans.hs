{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CologLearn.Trans
  where

import Colog.Core.Action (LogAction)
import Colog.Core (HasLog (..))
import Colog.Core.Severity
  ( pattern E, pattern I
  )
import Colog.Message
  ( Message
  , log, logDebug, logWarning
  )
import Colog.Monad (WithLog)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader
  ( MonadReader (..)
  , ReaderT
  , asks
  , runReaderT
  )
import Prelude hiding (log)


data Env m = Env
  { envSomeValue :: Int
  , envLogger :: LogAction m Message
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogger

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newAction env = env { envLogger = newAction }


newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env App))


runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env


example6 :: (MonadIO m, WithLog (Env App) Message m) => m ()
example6 = do
  -- Just some IO as in any transformer
  liftIO $ putStrLn "\nExample 6: A (ReaderT env IO a) transformer with the env instanced into HasLog"

  -- Accessing the ReaderT state
  liftIO . print =<< asks envSomeValue

  -- Accessing the co-log logger
  logDebug $ "example6: A debug message"
  log I $ "example6: This is an info message"
  logWarning $ "example6: A warning message"
  log E $ "example6: An error message"
