
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

import Linear (V2(..), V3(..), ortho, identity, perspective)

import Data.IORef
import Data.Array.Repa ((:.)(..)) -- Weirdest syntax ever

import           Graphics.Rendering.OpenGL hiding (perspective, Line, position, ortho, viewport)
import qualified Graphics.Rendering.OpenGL as GL

import           Graphics.UI.GLFW (MouseButton(..), MouseButtonState(..))
import qualified Graphics.UI.GLFW as GLFW

-- import Leibniz.Constants (pi)

import Graphics.Michelangelo.Types (Attribute(..), UniformValue(..))
import Graphics.Michelangelo.Shaders as Shader

import Maverick.Console as Console



--------------------------------------------------------------------------------------------------------------------------------------------

type Quad = ([(String, Attribute Int)], [(String, (UniformLocation, UniformValue Double Int))])
type App = (GLFW.Window, Console, Quad)

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
  -- depthTest $= Enabled
  depthFunc $= Just Lequal
  blend     $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D     $= ((Nearest, Nothing), Nearest)



-- Interaction -----------------------------------------------------------------------------------------------------------------------------

-- |
onmousepress :: IORef (GLFW.Window, Console, Quad) -> GLFW.MouseButtonCallback
onmousepress ref win button state mods = do
  return ()


-- |
onkeypress :: IORef (GLFW.Window, Console, Quad) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
onkeypress ref win key repeats keystate modifiers = do
  return ()


-- |
onmousemotion :: IORef (GLFW.Window, Console, Quad) -> GLFW.Window -> V2 Double -> IO ()
onmousemotion ref win m@(V2 mx my) = do
  return ()


-- |
onresize :: IORef (GLFW.Window, Console, Quad) -> GLFW.Window -> V2 Int -> IO ()
onresize ref win new = do
  GL.viewport $= (Position 0 0, Size cx cy)
  where
    (V2 cx cy) = fromIntegral <$> new

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: IORef (GLFW.Window, Console, Quad) -> IO ()
mainloop ref = do
  (Just t) <- GLFW.getTime
  frame ref 0 t


-- |
frame :: IORef (GLFW.Window, Console, Quad) -> Int -> Double -> IO ()
frame ref n lastTime = do
  (Just t) <- GLFW.getTime
  app <- readIORef ref
  render app
  GLFW.pollEvents
  -- maybe skip (runCommand ref) =<< tryTakeMVar (app^.command)
  unlessM (GLFW.windowShouldClose (fst app)) (frame ref (n+1) t)


-- |
render :: (GLFW.Window, Console, Quad) -> IO ()
render app = do
  return ()
  GLFW.swapBuffers (fst app)


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
quad :: V2 Double -> Console -> GL.Program -> IO (Either String Quad)
quad (V2 cx cy) console shader = runEitherT $ do
  lift (GL.currentProgram $= Just shader)
  attrs'    <- sequence [attr shader ("aTexCoord",       [V2 0 0,   V2 0 0,   V2 0 0,   V2 0 0]   :: [V2 Double]),
                         attr shader ("aVertexPosition", [V3 0 0 0, V3 0 0 0, V3 0 0 0, V3 0 0 0] :: [V3 Double])]

  uniforms' <- sequence [unif shader ("uSampler",  UInt 0),
                         unif shader ("uMVMatrix", UMatrix44 modelview),
                         unif shader ("uPMatrix",  UMatrix44 projection)]
  return (attrs', uniforms')
  where
    modelview  = (identity)
    projection = ortho
                   0  -- Left
                   cx -- Right
                   cy -- Bottom
                   0  -- Top
                   0  -- Near
                   1  -- Far

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
main = void . runEitherT $ do
  lift $ putStrLn "Let's see if this works"
  succeeded "Failed to create window" GLFW.init
  window' <- EitherT $ maybeToEither "Failed to create window" <$> createWindow (V2 720 480)

  lift $ do
    GLFW.makeContextCurrent (Just window') -- Not sure why this is needed or what it does
    prepareGraphics

  -- shader' <- EitherT (Shader.createProgram Shader.textured)
  shader <- EitherT (Shader.loadProgram Shader.texturedShaderPaths)
  lift $ GL.currentProgram $= Just shader


  lift $ do

    self  <- Console.new defaultSettings
    easel <- quad (V2 720 480) self shader
    ref   <- newIORef (window', self, easel)

    GLFW.setMouseButtonCallback window' $ Just (onmousepress  ref)
    GLFW.setKeyCallback         window' $ Just (onkeypress    ref)
    GLFW.setCursorPosCallback   window' $ Just (\win mx my -> onmousemotion ref win (V2 mx my))
    GLFW.setWindowSizeCallback  window' $ Just (\win cx cy -> onresize      ref win (V2 cx cy))
    
    mainloop ref

    GLFW.makeContextCurrent (Nothing) -- TODO: Is this necessary (?)
    GLFW.destroyWindow window'
    GLFW.terminate
  where
    -- TODO | - Make this not break
    root = "C:/Users/Jonatan/Desktop/Haskell/modules/maverick"