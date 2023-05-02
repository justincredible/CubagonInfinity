module Model (Model,
    modelPosition,
    getTransform ) where

import Data.Bifunctor
import Data.Text (pack)
import Graphics.GL
import Graphics.UI.GLFW
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import Linear.Vector

import Application.Control
import Application.Graphics
import Application.Parameters

data Model = Model {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei,
    getPosition :: V3 GLfloat,
    getRotation :: Quaternion GLfloat,
    getTransform :: M44 GLfloat,
    getAnimating :: Bool,
    getDirection :: Direction,
    getSpeed :: Double,
    getProgress :: GLfloat }
    deriving (Eq, Show)

instance Graphics Model where

    initialize (ModelTXT mdlFile) = loadTXT mdlFile
    initialize _ = return . Left . pack $ "Incorrect parameters."

    parameters = (const .) . const . return . Left . pack $ "Model does not implement parameters."

    render model = do
        glBindVertexArray (getVertexArray model)
        glDrawElements GL_TRIANGLES (getIndexCount model) GL_UNSIGNED_INT nullPtr
        (return . Right) model

    shutdown model = do
        glBindVertexArray (getVertexArray model)
        
        sequence_ $ map glDisableVertexAttribArray [0..2]
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with (getIndexBuffer model) $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with (getVertexBuffer model) $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with (getVertexArray model) $ glDeleteVertexArrays 1

data Direction = Stationary | East | South | West | North
    deriving (Eq, Show)

instance Control Model where

    control model (DeltaTime window delta) = if not $ getAnimating model
        then do
            east <- getKey window Key'Right
            south <- getKey window Key'Down
            west <- getKey window Key'Left
            north <- getKey window Key'Up
            case (east,south,west,north) of
                (KeyState'Pressed,_,KeyState'Pressed,_) -> return model
                (_,KeyState'Pressed,_,KeyState'Pressed) -> return model
                (KeyState'Pressed,_,_,_) -> return model { getAnimating = True, getDirection = East }
                (_,_,KeyState'Pressed,_) -> return model { getAnimating = True, getDirection = West }
                (_,KeyState'Pressed,_,_) -> return model { getAnimating = True, getDirection = South }
                (_,_,_,KeyState'Pressed) -> return model { getAnimating = True, getDirection = North }
                _ -> return model
        else do
            let stage = getProgress model
                prevspd = getSpeed model
                speed = min 10 $ case (stage < 0.1, stage < 1.0, stage < 1.5) of
                    (True,_,_) -> prevspd
                    (_,True,_) -> 4/3*prevspd
                    (_,_,True) -> 2*prevspd
                    _ -> prevspd
                progress = stage + realToFrac (speed*delta)
            if not $ progress > pi/2
            then do
                let (q,v) = case getDirection model of
                        East -> (axisAngle (V3 0 0 (-1)) progress, V3 (2*progress/pi) 0 0)
                        South -> (axisAngle (V3 1 0 0) progress, V3 0 0 (2*progress/pi))
                        West -> (axisAngle (V3 0 0 1) progress, V3 (-2*progress/pi) 0 0)
                        North -> (axisAngle (V3 (-1) 0 0) progress, V3 0 0 (-2*progress/pi))
                        _ -> undefined
                    transform = mkTransformation (q*getRotation model) (v ^+^ Model.getPosition model)
                return model { getTransform = transform, getProgress = progress, getSpeed = speed }
            else do
                east <- getKey window Key'Right
                south <- getKey window Key'Down
                west <- getKey window Key'Left
                north <- getKey window Key'Up
                let (q,v) = bimap (* getRotation model) (^+^ Model.getPosition model) $ case getDirection model of
                        East -> (axisAngle (V3 0 0 1) (-pi/2), V3 1 0 0)
                        South -> (axisAngle (V3 1 0 0) (pi/2), V3 0 0 1)
                        West -> (axisAngle (V3 0 0 1) (pi/2), V3 (-1) 0 0)
                        North -> (axisAngle (V3 1 0 0) (-pi/2), V3 0 0 (-1))
                        _ -> undefined
                    (direction,momentum,animate) = case (east,south,west,north) of
                        (KeyState'Pressed,_,KeyState'Pressed,_) -> (Stationary,1,False)
                        (_,KeyState'Pressed,_,KeyState'Pressed) -> (Stationary,1,False)
                        (KeyState'Pressed,_,_,_) -> (East,speed,True)
                        (_,_,KeyState'Pressed,_) -> (West,speed,True)
                        (_,KeyState'Pressed,_,_) -> (South,speed,True)
                        (_,_,_,KeyState'Pressed) -> (North,speed,True)
                        _ -> (Stationary,1,False)
                return model { getRotation = q, Model.getPosition = v,
                    getTransform = mkTransformation q v, getProgress = 0,
                    getAnimating = animate, getDirection = direction, getSpeed = momentum }
    control model _ = putStrLn "Mismatched parameters for Model." >> return model

loadTXT :: FilePath -> IO (Either a Model)
loadTXT file = do
    (numVertices,vertices) <- fmap read . readFile $ file :: IO (Int,[GLfloat])
    let indices = [0..fromIntegral numVertices - 1] :: [GLuint]
        numIndices = numVertices
    
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = sizeOf (head vertices)*quot (length vertices) numVertices
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices*vertexSize) ptr GL_STATIC_DRAW
    
    sequence_ $ map glEnableVertexAttribArray [0..2]
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf(GL_FLOAT))
    glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (5*sizeOf(GL_FLOAT))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    glBindVertexArray 0
    
    return . Right $ Model
        vertexArray vertexBuffer indexBuffer (fromIntegral numIndices)
        (V3 0 0 0) (Quaternion 1 (V3 0 0 0)) identity
        False Stationary 1 0
    where
    bufferOffset = plusPtr nullPtr

modelPosition :: Model -> V3 GLfloat
modelPosition = getPosition