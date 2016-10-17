
module Main where

import Text.Printf

import Control.Applicative (liftA2)
import Control.Monad.Trans.State as State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_, forM, liftM, unless, forever, void, when, (<=<))
import Control.Monad.Extra (unlessM)

import Control.Lens

import Foreign.Storable

import Linear (V2(..), V3(..), M44, ortho, identity, perspective, translation)

import qualified Data.Map as M
import Data.IORef
import Data.Array.Repa ((:.)(..)) -- Weirdest syntax ever

import           Graphics.Rendering.OpenGL hiding (perspective, Line, position, ortho, viewport, texture)
import qualified Graphics.Rendering.OpenGL.GL.Shaders as GL
import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.GLUtil         as GL --
import           Graphics.GLUtil.GLError as GL

import           Graphics.UI.GLFW (MouseButton(..), MouseButtonState(..))
import qualified Graphics.UI.GLFW as GLFW

-- import Leibniz.Constants (pi)

import           Graphics.Michelangelo.Shapes (planeXY, triangles)
import qualified Graphics.Michelangelo.Lenses as L
import           Graphics.Michelangelo.Types (Attribute(..), UniformValue(..), ShaderPaths(..), Matrix44(..))
import           Graphics.Michelangelo.Shaders as Shader

import Cartesian.Core (size, x, y, z)

import Maverick.Console as Console



--------------------------------------------------------------------------------------------------------------------------------------------

type Quad = (M.Map String (Attribute Int), M.Map String (UniformLocation, UniformValue Double Int))
type App = (GLFW.Window, Console, Quad, GL.Program)

windowOf  :: Simple Lens App GLFW.Window 
windowOf = _1

consoleOf :: Simple Lens App Console
consoleOf = _2

easelOf :: Simple Lens App Quad
easelOf = _3

shaderOf :: Simple Lens App GL.Program
shaderOf = _4

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Takes an action that signals success with a Bool, and turns it into an EitherT value
--   with an error message.
-- TODO | - Rename
succeeded :: String -> IO Bool -> EitherT String IO ()
succeeded message action = do
  success <- lift action
  EitherT . return $ fromBool message () success


-- |
maybeToEither :: err -> Maybe a -> Either err a
maybeToEither err ma = maybe (Left err) (Right) ma


-- | 
fromBool :: err -> a -> Bool -> Either err a
fromBool _   a True  = Right a
fromBool err _ False = Left err

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
prepareGraphics :: IO ()
prepareGraphics = do
  -- GL.depthTest $= Disabled
  -- depthFunc $= Just Lequal
  debugGL

  blend     $= Disabled
  -- blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  debugGL

  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D     $= ((Nearest, Nothing), Nearest)
  debugGL


-- | 
debugGL :: IO ()
debugGL = GL.errors >>= \e -> when (not $ null e) (print e)

-- Interaction -----------------------------------------------------------------------------------------------------------------------------

-- |
onmousepress :: IORef App -> GLFW.MouseButtonCallback
onmousepress ref win button state mods = do
  return ()


-- |
onkeypress :: IORef App -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
onkeypress ref win key repeats keystate modifiers = do
  return ()


-- |
onmousemotion :: IORef App -> GLFW.Window -> V2 Double -> IO ()
onmousemotion ref win m@(V2 mx my) = do
  return ()


-- |
onresize :: IORef App -> GLFW.Window -> V2 Int -> IO ()
onresize ref win new = do
  GL.viewport $= (Position 0 0, Size cx cy)
  where
    (V2 cx cy) = fromIntegral <$> new

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: IORef App -> IO ()
mainloop ref = do
  (Just t) <- GLFW.getTime
  frame ref 0 t


-- |
frame :: IORef App -> Int -> Double -> IO ()
frame ref n lastTime = do
  (Just t) <- GLFW.getTime
  debugGL

  app <- readIORef ref
  render app (realToFrac t)
  GLFW.pollEvents
  -- maybe skip (runCommand ref) =<< tryTakeMVar (app^.command)
  debugGL

  unlessM (GLFW.windowShouldClose (app^.windowOf)) (frame ref (n+1) t)


-- |
render :: App -> Float -> IO ()
render app t = do
  let (V2 cx cy) = V2 720 480
  debugGL

  GL.currentProgram $= Just (app^.shaderOf)
  debugGL

  GL.viewport $= (Position 0 0, let (V2 cx' cy') = fmap floor (V2 cx cy) in Size cx' cy')
  GL.clearColor $= Color4 0.15 0.68 0.90 1.00
  GL.clear [ColorBuffer, DepthBuffer]
  debugGL
  
  GL.activeTexture  $= (GL.TextureUnit 0) -- 
  GL.textureBinding GL.Texture2D $= Just (app^.consoleOf.canvas.texture) -- Is this needed (?)
  
  -- TODO: We shouldn't hard-code the wrapping and filtering operations
  -- GL.texture2DWrap $= (Repeated, ClampToEdge)
  -- GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
  debugGL

  loc1 <- GL.get (GL.uniformLocation (app^.shaderOf) "uMVMatrix")
  GL.uniform loc1 $= (Matrix44 (identity & translation.z .~ (-2) :: M44 GL.GLfloat))
  debugGL

  loc2 <- GL.get (GL.uniformLocation (app^.shaderOf) "uPMatrix")
  GL.uniform loc2 $= (Matrix44 (ortho (-cx/2) (cx/2) (-cy/2) (cy/2) 0 20 :: M44 GL.GLfloat))
  debugGL

  loc3 <- GL.get (GL.uniformLocation (app^.shaderOf) "uSampler")
  GL.uniform loc3 $= (0 :: GL.GLint)
  debugGL

  forM (app^.easelOf._1) $ \attr -> do
    debugGL
    GL.vertexAttribArray (attr^.L.location)   $= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer              $= Just (attr^.L.buffer)                              
    debugGL
    GL.vertexAttribPointer (attr^.L.location) $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral $ attr^.L.count) GL.Float 0 GL.offset0)

  -- Shader.bindAttributes (app^.easelOf._1)
  -- Shader.bindUniforms   (app^.easelOf._2)
  -- GL.printError

  debugGL
  GL.drawArrays (GL.Triangles) 0 (6)
  debugGL
  
  GLFW.swapBuffers (app^.windowOf)

  debugGL

  where
    -- | generalisation of 'mod' to any instance of Real
    mod' :: (Real a) => a -> a -> a
    mod' n d = n - (fromInteger f) * d where
        f = div' n d

    -- | generalisation of 'div' to any instance of Real
    div' :: (Real a,Integral b) => a -> a -> b
    div' n d = floor ((toRational n) / (toRational d))


-- |
attr :: (Foldable t, Foldable v, Storable f, Real f) => GL.Program -> (String, t (v f)) -> EitherT String IO (String, Attribute Int)
attr program' = EitherT . uncurry (Shader.newAttribute program')


-- |
unif :: GL.Program -> (String, UniformValue f i) -> EitherT String IO (String, (UniformLocation, UniformValue f i))
unif program' (name, value) = EitherT $ (ensureLocation) <$> (GL.get . uniformLocation program' $ name)
  where
    ensureLocation (UniformLocation (-1)) = Left  $ printf "Could not find uniform location '%s'" name
    ensureLocation loc                    = Right $ (name, (loc, value))


-- |
quad :: V2 Double -> Console -> GL.Program -> EitherT String IO Quad
quad (V2 cx cy) console shader = do
  lift $ do
    debugGL
    GL.currentProgram $= Just shader
    print $ planeXY (\x y _ -> V2 (x+0.5) (y+0.5)) 1  1
    print $ planeXY (\x y z -> V3 x y (-0.5)) dx dy

  attrs'    <- sequence [attr shader ("aTexCoord",       concat . triangles $ planeXY (\x y _ -> V2 (x+0.5) (y+0.5))    1  1 :: [V2 Double]),
                         attr shader ("aVertexPosition", concat . triangles $ planeXY (\x y z -> V3 x y (-0.5)) dx dy :: [V3 Double])]

  lift (debugGL)

  uniforms' <- sequence [unif shader ("uSampler",  UInt 0),
                         unif shader ("uMVMatrix", UMatrix44 modelview),
                         unif shader ("uPMatrix",  UMatrix44 projection)]

  lift (debugGL)

  return (M.fromList attrs', M.fromList uniforms')
  where
    (V2 dx dy) = fromIntegral <$> (console^.viewport.size)
    modelview  = (identity & translation .~ (V3 0 0 0))
    projection = ortho
                   0    -- Left
                   cx   -- Right
                   0    -- Bottom
                   cy   -- Top
                   0.1  -- Near
                   10   -- Far

    -- projection = (perspective
    --                (torad 40.0) -- FOV (y direction, in radians)
    --                1.0          -- Aspect ratio
    --                1.0          -- Near plane
    --                80.0)        -- Far plane

-- |
createWindow :: V2 Int -> IO (Maybe GLFW.Window)
createWindow (V2 cx cy) = GLFW.createWindow cx cy "Maverick Console (2016)" Nothing Nothing

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = print <=< runEitherT $ do
  lift $ putStrLn "Let's see if this works"
  succeeded "Failed to create window" GLFW.init
  
  lift $ do
    GLFW.defaultWindowHints
    GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
  window' <- EitherT $ maybeToEither "Failed to create window" <$> createWindow (V2 720 480)

  lift $ do
    GLFW.makeContextCurrent (Just window') -- Not sure why this is needed or what it does
    prepareGraphics
    debugGL

  -- shader' <- EitherT (Shader.createProgram Shader.textured)
  shader <- EitherT (Shader.loadProgram ShaderPaths { fVertex = root ++ "/assets/shaders/shader-textured-vertex.glsl",
                                                      fPixel  = root ++ "/assets/shaders/shader-textured-pixel.glsl" })
  lift (debugGL)

  lift $ GL.currentProgram $= Just shader
  lift (debugGL)

  self  <- lift $ Console.new defaultSettings
  easel <- quad (V2 720 480) self shader
  ref   <- lift $ newIORef (window', self, easel, shader)

  lift $ do

    GLFW.setMouseButtonCallback window' $ Just (onmousepress  ref)
    GLFW.setKeyCallback         window' $ Just (onkeypress    ref)
    GLFW.setCursorPosCallback   window' $ Just (\win mx my -> onmousemotion ref win (V2 mx my))
    GLFW.setWindowSizeCallback  window' $ Just (\win cx cy -> onresize      ref win (V2 cx cy))
    GLFW.setErrorCallback (Just $ \e s -> print e >> putStrLn s)
    
    mainloop ref

    GLFW.makeContextCurrent (Nothing) -- TODO: Is this necessary (?)
    GLFW.destroyWindow window'
    GLFW.terminate
  where
    -- TODO | - Make this not break
    root = "C:/Users/Jonatan/Desktop/Haskell/modules/maverick"