module Engine (run) where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.UI.GLFW
import System.Win32.Info
import System.Win32.Info.Computer

import Application.Graphics
import Application.Parameters
import Frame

data Engine = Engine {
    getWindow :: Window,
    getFrame :: Frame,
    getScrollPtr :: Ptr Double }
    deriving (Eq, Show)

run :: IO ()
run = do
    _ <- Graphics.UI.GLFW.init
    
    let width = 800
        height = 600
    (window,(scrwidth,scrheight)) <- openWindow "Cubagon Infinity" width height
    
    glEnable GL_CULL_FACE
    glEnable GL_DEPTH_TEST
    glEnable GL_MULTISAMPLE
    
    ptr <- malloc
    setScrollCallback window (Just $ scrollCallback ptr)
    initialize (Framing window scrwidth scrheight ptr) >>= either ((>>) . print <*> const (close window)) (start ptr window)
    where
    start ptr window frame = loop (Engine window frame ptr) >> end (Engine window frame ptr)
    continue engine frame = loop engine { getFrame = frame }
    loop engine@(Engine window frame _) = do
        result <- render frame
        
        pollEvents
        
        escape <- getKey window Key'Escape
        quit <- windowShouldClose window
        when (escape /= KeyState'Pressed && not quit) $ either print (continue engine) result

openWindow :: String -> Int -> Int -> IO (Window, (Int, Int))
openWindow title width height = do
    defaultWindowHints
    windowHint (WindowHint'ContextVersionMajor 4)
    windowHint (WindowHint'ContextVersionMinor 6)
    windowHint (WindowHint'Resizable False)
    windowHint (WindowHint'Decorated False)
    windowHint (WindowHint'Samples (Just 4))
    
    Just window <- createWindow width height title Nothing Nothing
    
    makeContextCurrent (Just window)
    
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    
    setCursorInputMode window CursorInputMode'Disabled
    
    setCursorPos window (fromIntegral $ quot screenW 2) (fromIntegral $ quot screenH 2)
    
    return (window,(screenW,screenH))

end :: Engine -> IO ()
end (Engine window frame ptr) = free ptr >> shutdown frame >> close window

close :: Window -> IO ()
close window = destroyWindow window >> terminate

scrollCallback :: (Storable a, Num a) => Ptr a -> p1 -> p2 -> a -> IO ()
scrollCallback ptr _window _dx dy = do
    d <- peek ptr
    poke ptr (d+dy)
