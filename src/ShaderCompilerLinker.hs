module ShaderCompilerLinker (
    compileAndLink,
    outputShaderErrorMessage,
    outputLinkerErrorMessage)
    where

import Prelude hiding (readFile)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Text hiding (map)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

compileAndLink :: FilePath -> FilePath -> IO (Either Text GLuint)
compileAndLink vsFile fsFile = do
    vsBuffer <- (fmap (map fromIntegral . BS.unpack) . BS.readFile $ vsFile) :: IO [GLchar]
    fsBuffer <- (fmap (map fromIntegral . BS.unpack) . BS.readFile $ fsFile) :: IO [GLchar]
    
    vShader <- glCreateShader GL_VERTEX_SHADER
    fShader <- glCreateShader GL_FRAGMENT_SHADER
    
    withArray0 0 vsBuffer $ flip with (flip (glShaderSource vShader 1) nullPtr)
    
    withArray0 0 fsBuffer $ flip with (flip (glShaderSource fShader 1) nullPtr)
    
    glCompileShader vShader
    glCompileShader fShader
    
    statusv <- alloca $ (>>) . glGetShaderiv vShader GL_COMPILE_STATUS <*> peek
    when (statusv /= 1) $
        outputShaderErrorMessage vShader vsFile >> putStrLn "Error compiling vertex shader."
    
    statusf <- alloca $ (>>) . glGetShaderiv fShader GL_COMPILE_STATUS <*> peek
    when (statusf /= 1) $
        outputShaderErrorMessage fShader fsFile >> putStrLn "Error compiling fragment shader."
    
    if statusv /= 1 || statusf /= 1
    then do
        glDeleteShader vShader
        glDeleteShader fShader
        
        return . Left . pack $ "Error in GLSL source."
    else do
        program <- glCreateProgram
        
        glAttachShader program vShader
        glAttachShader program fShader
            
        glLinkProgram program
        
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        status <- alloca $ (>>) . glGetProgramiv program GL_LINK_STATUS <*> peek
        if (status /= 1)
        then do
            outputLinkerErrorMessage program
            glDeleteProgram program
            return . Left . pack $ "Error linking shader program."
        else return . Right $ program

outputShaderErrorMessage :: GLuint -> String -> IO ()
outputShaderErrorMessage shaderID shaderFile = do
    logSize <- alloca $ (>>) . glGetShaderiv shaderID GL_INFO_LOG_LENGTH <*> peek
    
    let logSizeI = fromIntegral logSize
    infoLog <- allocaArray logSizeI $ (>>) . glGetShaderInfoLog shaderID logSize nullPtr <*> peekArray logSizeI
    
    writeFile "shader-error.txt" (shaderFile ++ ":\n" ++ map castCCharToChar infoLog)

outputLinkerErrorMessage :: GLuint -> IO ()
outputLinkerErrorMessage programID = do
    logSize <- alloca $ (>>) . glGetProgramiv programID GL_INFO_LOG_LENGTH <*> peek
    
    let logSizeI = fromIntegral logSize
    infoLog <- allocaArray logSizeI $ (>>). glGetProgramInfoLog programID logSize nullPtr <*> peekArray logSizeI
    
    writeFile "linker-error.txt" $ map castCCharToChar infoLog
