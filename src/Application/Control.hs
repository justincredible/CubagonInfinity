module Application.Control (Control(..)) where

import Application.Parameters

class Control a where
    control :: a -> ControlParams -> IO a
