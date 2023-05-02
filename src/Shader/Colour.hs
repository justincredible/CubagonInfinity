module Shader.Colour (ColourShader) where

import Data.Text (pack)
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Application.Graphics
import Application.Parameters
import ShaderCompilerLinker

data ColourShader = Colour {
    getProgram :: GLuint,
    getMVPLocation :: GLint,
    getTextureLocation :: GLint }
    deriving (Eq, Show)

instance Graphics ColourShader where

    initialize Shader = compileAndLink "glsl/colour.vertex" "glsl/colour.fragment"
        >>= either ((>>) . print <*> (const . return . Left . pack) "Error building shader program.") success
        where
        success program = do
            sequence_ $ zipWith ((. (withArray . map castCharToCChar)) . flip ($) . glBindAttribLocation program)
                [0..]
                ["position","texcoord","normal"]
            
            uniforms@[mvp,tex] <- sequence $
                map (flip (withArray0 0) (glGetUniformLocation program)) (map (map castCharToCChar) ["mvp","tex"])
            
            if any (== -1) uniforms
            then do
                glDeleteProgram program
                return . Left . pack $ "Error fetching uniform location" ++ show uniforms
            else return . Right $ Colour program mvp tex
    initialize _ = return . Left . pack $ "Incorrect parameters."

    parameters (Colour program mvp _tex) (ShaderColour mvpMx) = do
        glUseProgram program

        withArray mvpMx $ glUniformMatrix4fv mvp 1 GL_FALSE
        
        return . Right $ ()
    parameters _ _ = return . Left . pack $ "Incorrect parameters."

    render = const . return . Left . pack $ "Shaders do not implement render."

    shutdown = glDeleteProgram . getProgram
