module Application.Graphics (Graphics(..)) where

import Data.Text (Text,pack,append)

import Application.Parameters

class Graphics a where
    initialize :: InitParams -> IO (Either Text a)
    parameters :: a -> SetParams -> IO (Either Text ())
    render :: a -> IO (Either Text a)
    shutdown :: a -> IO ()

instance Graphics a => Graphics (Maybe a) where
    initialize = fmap (fmap Just) . initialize
    parameters a p = maybe nullArg (flip parameters p) a
    render = fmap (fmap Just) . maybe nullArg render
    shutdown = maybe (return ()) shutdown

instance (Graphics a, Show e) => Graphics (Either e a) where
    initialize = fmap (fmap Right) . initialize
    parameters a p = either (errorArg . show) (flip parameters p) a
    render = fmap (fmap Right) . either (errorArg . show) render
    shutdown = either (const $ return ()) shutdown
    
nullArg = return . Left . pack $ "Null argument."
errorArg = return . Left . append (pack "Error argument:\n") . pack
