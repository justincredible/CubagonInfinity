module Frame (Frame) where

import Data.Foldable
import Data.Text (pack)
import Graphics.GL
import Graphics.UI.GLFW
import Linear.Epsilon
import Linear.Matrix
import Linear.Projection
import Linear.V3
import Linear.Vector

import Application.Control
import Application.Graphics
import Application.Parameters
import Camera
import Model
import Shader.Colour

data Frame = Frame {
    getWindow :: Window,
    getLastTime :: Double,
    getCamera :: Camera,
    getModel :: Model,
    getShader :: ColourShader }
    deriving (Eq, Show)

instance Graphics Frame where

    initialize (Framing window scrwidth scrheight ptr) = let camera = defaultCamera scrwidth scrheight ptr in
        initialize (ModelTXT "res/model/cube.txt")
        >>= either ((>>) . print <*> (const . return . Left . pack) "Error creating model.") (success camera)
        where
        success camera model = do
            Just time <- getTime
            shader <- initialize Shader
            either (failure model) (return . Right . Frame window time camera model) shader
        failure model message = do
            shutdown model
            print message
            return . Left . pack $ "Error creating shader."
    initialize _ = return . Left . pack $ "Incorrect parameters."

    parameters = (const .) . const . return . Left . pack $ "Frame does not implement parameters."

    render frame = do
        glClearColor 0 0 0 1
        glClear $ GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT
        
        Just time <- getTime
        let deltaTime = time - (getLastTime frame)
            window = getWindow frame
        
        model <- control (getModel frame) (DeltaTime window deltaTime)
        camera <- control (getCamera frame) (DeltaTime window deltaTime)
        
        let matrix = matrixToList . transpose $
                getProjection camera
                !*! getView camera
                !*! getTransform model
        parameters (getShader frame) (ShaderColour matrix)

        render model

        swapBuffers window

        return . Right $ frame { getLastTime = time, getCamera = camera, getModel = model }
        where
        matrixToList = concat . map toList . toList

    shutdown = (>>) . shutdown . getShader <*> shutdown . getModel
