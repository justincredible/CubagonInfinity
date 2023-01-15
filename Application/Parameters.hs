module Application.Parameters where

import Data.Int
import Data.Word
import Foreign.Ptr
import Graphics.GL
import Graphics.UI.GLFW
import Linear.V3

data InitParams = Undecided
    | Shader
    | Framing Window Int Int (Ptr Double)
    | ModelTXT String
    deriving (Eq, Show)

data SetParams = Identity
    | ShaderColour [Float]
    deriving (Eq, Show)

data ControlParams = Self
    | DeltaTime Window Double
    | CameraPositioning Window Double (V3 Float)
    deriving (Eq, Show)
