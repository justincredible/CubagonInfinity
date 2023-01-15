module Application.Control (Control(..)) where

import Data.Text (pack)

import Application.Parameters

class Control a where
    control :: a -> ControlParams -> IO a
