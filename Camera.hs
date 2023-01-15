module Camera (
    Camera,
    defaultCamera,
    getView,
    getProjection,
    currentPosition,
    currentHAngle,
    currentVAngle ) where

import Data.Text (pack)
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.GLFW
import Linear.Epsilon
import Linear.Matrix
import Linear.Projection
import Linear.Quaternion
import Linear.V3
import Linear.Vector

import Application.Control
import Application.Parameters
import Model

mouseSpeed = 0.005
speed = 3.0

data Camera = Camera {
    getScreenWidth :: Int,
    getScreenHeight :: Int,
    getView :: M44 Float,
    getProjection :: M44 Float,
    getPosition :: V3 Float,
    getHAngle :: Float,
    getVAngle :: Float,
    getFOV :: Float,
    getScrollPtr :: Ptr Double }
    deriving (Eq, Show)

defaultCamera width height ptr = Camera width height
    identity identity
    (V3 0 0 0)
    (-3*pi/4) (-pi/5)
    (pi/4) ptr

instance Control Camera where
    control camera (DeltaTime window time) = do
        (posx, posy) <- getCursorPos window
        (width,height) <- getWindowSize window
        
        let hZero = fromIntegral $ getScreenWidth camera `quot` 2
            vZero = fromIntegral $ getScreenHeight camera `quot` 2
        setCursorPos window hZero vZero
        
        let horizontalAngle = getHAngle camera + realToFrac (mouseSpeed * (hZero - posx))
            hAngle = horizontalAngle + if horizontalAngle < 0
                then 2*pi
                else if horizontalAngle > 2*pi
                    then -2*pi
                    else 0
            
            verticalAngle = getVAngle camera + realToFrac (mouseSpeed * (vZero - posy))
            vAngle = max (-pi/2) $ min (pi/2) verticalAngle
            
            direction = V3 (cos vAngle*sin hAngle) (sin vAngle) (cos vAngle*cos hAngle)
            right = V3 (sin $ hAngle - pi/2) 0 (cos $ hAngle - pi/2)
            up = cross right direction

        let position = V3 5 5 5
            ptr = getScrollPtr camera

        delta <- peek ptr
        poke ptr 0
        
        home <- getKey window Key'Home
        let d = 10*realToFrac (time*delta)
            fov = if home == KeyState'Pressed then pi/4 else min (pi/2) $ max (pi/12) (getFOV camera - d)
        
        return $ camera {
            getView = lookAt position (position ^+^ direction) up,
            getProjection = perspective fov (fromIntegral width/fromIntegral height) 0.1 100,
            getHAngle = hAngle,
            getVAngle = vAngle,
            getFOV = fov }
        
        where
        zeroOrValue a = if nearZero a then 0 else a
    control camera _ = putStrLn "Mismatched parameters for Camera." >> return camera

currentPosition camera = getPosition camera
currentHAngle camera = "Ha: " ++ (show . getHAngle) camera
currentVAngle camera = "Va: " ++ (show . getVAngle) camera
