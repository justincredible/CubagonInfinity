module Engine (run) where

import Control.Monad
import Data.Foldable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.Gloss.Interface.Environment
import Graphics.UI.GLFW

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
    glEnable GL_DEBUG_OUTPUT

    callback <- makeGLDEBUGPROC debugMessageCallback
    glDebugMessageCallback callback nullPtr

    ptr <- malloc
    setScrollCallback window (Just $ scrollCallback ptr)
    initialize (Framing window scrwidth scrheight ptr) >>= either ((>>) . print <*> const (close window)) (start ptr window callback)
    where
    start ptr window callback frame = loop (Engine window frame ptr) >> end (Engine window frame ptr) callback
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
    windowHint (WindowHint'OpenGLDebugContext True)
    
    Just window <- createWindow width height title Nothing Nothing
    
    makeContextCurrent (Just window)
    
    (screenW, screenH) <- getScreenSize
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    
    setCursorInputMode window CursorInputMode'Disabled
    
    setCursorPos window (fromIntegral $ quot screenW 2) (fromIntegral $ quot screenH 2)
    
    return (window,(screenW,screenH))

end :: Engine -> GLDEBUGPROC -> IO ()
end (Engine window frame ptr) funPtr = freeHaskellFunPtr funPtr
    >> free ptr
    >> shutdown frame
    >> close window

close :: Window -> IO ()
close window = destroyWindow window >> terminate

scrollCallback :: (Storable a, Num a) => Ptr a -> p1 -> p2 -> a -> IO ()
scrollCallback ptr _window _dx dy = do
    d <- peek ptr
    poke ptr (d+dy)

debugMessageCallback :: GLDEBUGPROCFunc
debugMessageCallback source errType errId severity length message userParam = do
    (_, dl) <- foldrM f (message, id) [1..length]
    putStrLn $ dl []
    where
    f _ (ptr, dl) = do
        c <- peek ptr
	return (plusPtr ptr 1, dl . (castCCharToChar c:))
