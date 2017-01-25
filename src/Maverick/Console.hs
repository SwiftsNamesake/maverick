
--
-- Pixels.Console
-- Windowless console without all the legacy cruft
--
-- Jonatan H Sundqvist
-- September 14 2016
--

-- TODO | - Input
--        - Functional reactive programming integration
--        - Separate the 'console' (eg. pure text rendering) from the 'command prompt'
--        - [Lens-based] interface that hides implementation (eg. setting the 'dirty' flag)

-- SPEC | - For simplicity - and to avoid bikeshedding - I have decided to use reasonable defaults for the types of Fonts, Indices, Buffers
--          and encodings. I may reconsider that choice once I have a working implementation of this module.
--          


------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedRecordFields #-} -- Some day...
{-# LANGUAGE DuplicateRecordFields  #-} -- PRAISE BE THE LORD FOR THIS BLESSING
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Maverick.Console where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll neeed these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStr, putStrLn, putChar)
import qualified Prelude as P

import GHC.Stack

import Linear (V2(..), V3(..))

import Control.Lens (makeFields, makeLensesWith, abbreviatedFields, Getter, Setter, Lens, (.~), (^.))
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.Either

-- import Foreign.Storable.Tuple ()
-- import Foreign.Ptr as Ptr

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Word
import qualified Data.Map as M

import           Data.Colour
import           Data.Colour.SRGB
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import           Data.Array.Repa ((:.)(..)) -- Weirdest syntax ever
import qualified Data.Array.Repa as R

import qualified Graphics.Text.TrueType as TT
import           Graphics.Rendering.OpenGL as GL hiding (projection, perspective, Line, position, ortho, viewport, RGB)

-- import Interpolate
import Cartesian.Core (BoundingBox(..), left, right, top, corner, size, bottom)

import Graphics.Michelangelo.Types (Mesh(..))
import Graphics.Michelangelo.Texture as Texture



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- Types ---------------------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Proper anti-aliased fonts with full Unicode support (mono-spaced)
--          cf. https://medium.com/@evanwallace/easy-scalable-text-rendering-on-the-gpu-c3f4d782c5ac#.ct9d4tfvp
--
--        - [Viewport] Use Natural type, Cartesian.BoundingBox (?)
--        - [Settings] I'll add more options eventually, I'm sure
--        - [Console]  I'll keep this structure for now, but we'll probably have to change the buffer type for efficiency
--
--        - Should I include metadata with the fonts (?)

-- |
-- TODO: Graphics primitives
type Texture = GL.TextureObject -- TODO: Use frame buffer object (optional?) (?)
type RGBA w  = (w, w, w, w)


-- |
type Viewport = BoundingBox (V2 Int)


-- |
data FontMap = FontMap {
  fFindGlyph :: (Char -> Maybe Viewport),
  fGlyphSize :: V2 Int,
  fTexture   :: Texture
}


-- |
data Font = Font {
  fFamily :: (),
  fSize   :: V2 Int
}


-- |
data Canvas = Canvas {
  fTexture :: Texture, -- TODO: Rename (this is the output texture)
  fDirty   :: Bool
}


-- |
data Settings = Settings {
  fFont       :: Font,
  fBackground :: RGBA Word8,
  fResizable  :: Bool,
  fAutoscroll :: Bool
}


-- |
data Cursor = Cursor {
  fRow    :: Int,
  fColumn :: Int
}


-- |
data Console  = Console {
  fBuffer   :: Text,
  fCursor   :: Cursor,
  fCanvas   :: Canvas,
  fViewport :: Viewport,
  fSettings :: Settings
}


-- |
-- TODO: Lenses (✓)
-- makeLensesWith (defaultFieldRules & lensField .~ yourCustomFieldNamingConvention)
makeLensesWith abbreviatedFields ''Colour
makeLensesWith abbreviatedFields ''Settings
makeLensesWith abbreviatedFields ''Cursor
makeLensesWith abbreviatedFields ''Canvas
makeLensesWith abbreviatedFields ''Console


-- TODO | - Font introspection, adding new ones
--        - Saving state, resuming sessions
--        - Formatting (cf. interpolation)
--        - Events, hooks, FRP, plugins
--        - Custom backends (?)
--        - Tabs
--        - Multiple viewports (for the same buffer)

-- Initialisation ------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Maybe add some settings (?)
new :: Settings -> IO Console
new config = do
  tex <- newCanvas (bounds) (config^.background)
  return $ Console { fBuffer   = "",
                     fCursor   = Cursor 0 0,
                     fCanvas   = Canvas { fTexture = tex, fDirty = False },
                     fViewport = bounds,
                     fSettings = config }
  where
    bounds = BoundingBox { cornerOf = V2 0 0, sizeOf = V2 120 40 }


-- |
defaultSettings :: Settings
defaultSettings = Settings { fBackground = (0, 0, 0, 0),
                             fResizable  = False,
                             fAutoscroll = False,
                             fFont       = Font { fSize = 13, fFamily = () } }

-- Rendering -----------------------------------------------------------------------------------------------------------------------------------------

-- |
newCanvas :: Viewport -> RGBA Word8 -> IO Texture
newCanvas view bg = do
  print (view^.size)
  tex <- Texture.createRepaTexture (view^.size) (\_ -> bg)
  return tex


-- |
-- TODO | - Reconsider name (eg. renderBuffer[Of])
--        - Render the actual text (:/)
-- render :: Console -> IO Console


-- | 
-- refresh :: Console -> IO ()
-- refresh self = _

-- Text rendering ------------------------------------------------------------------------------------------------------------------------------------

-- |
-- createFontMap :: Font -> IO FontMap
-- createFontMap = ()


-- Fonts ---------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- fonts :: EitherT String IO [GL.TextureObject]
-- fonts :: FilePath -> EitherT String IO (M.Map String TT.Font)
-- fonts root = M.fromList <$> (sequencePairs $ map (load <$>) [("Elixia",    "Elixia.ttf"),
--                                                              ("rothenbg",  "rothenbg.ttf"), 
--                                                              ("digital-7", "digital-7 (mono).ttf")])
--   where
--     load :: FilePath -> EitherT String IO TT.Font
--     load = EitherT . TT.loadFontFile . (root </>)


-- |
-- TODO | - Tessellate (on the GPU?)
--          cf. https://hackage.haskell.org/package/OpenGL-2.1/docs/Graphics-Rendering-OpenGL-GLU-Tessellation.html#t:AnnotatedVertex
--        - Options
--        - Metrics
-- createFontTexture :: Font -> String -> IO FontMap
-- createFontTexture font charset = _
--   where
--     vertices' :: [[V3 Float]]
--     vertices' = map makeGlyphVertices glyphs
    
--     -- I'm assuming each V.Vector holds the coordinates for a connected line strip
--     -- and that each glyph is represented by a list of such vectors (some glyphs are of course disjoint).
--     --
--     -- The docs do not make this point explicitly so I'll just have to test it. 
--     makeGlyphVertices :: [V.Vector (Float, Float)] -> [V3 Float]
--     makeGlyphVertices glyph = concatMap stitch glyph

--     -- | I also considered the names 'pairs', 'pairwise' and 'connect'
--     -- TODO | - Be strict (remind me why foldl is bad?)
--     --        - Account for empty Vector (?)
--     stitch :: V.Vector (Float, Float) -> [V3 Float]
--     stitch = drop 1 . reverse . drop 1 . V.foldl (\xs p -> let v = fromTuple p in v:v:xs) []

--     fromTuple :: Num a => (a, a) -> V3 a
--     fromTuple (x, y) = V3 x (-y) 0

--     texcoords' = map (\(V3 x y z) -> V2 (x/1) (y/1)) (concat vertices') -- TODO: Do this properly?
--     colours'   = concat $ zipWith (\c g -> replicate (length g) c) (cycle [V4 0.2 0.2 0.9 1, V4 0.9 0.35 0.42 1.0]) vertices' --
--     glyphs = TT.getStringCurveAtPoint 508 (0,0) [(font', TT.PointSize 48, s)] -- TODO: Figure out exactly what each argument means

-- Output --------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- putChar  :: Console -> Char -> IO ()
-- putStr   :: Console -> Text -> IO ()
-- putStrLn :: Console -> Text -> IO ()

-- print :: Show a => Console -> a -> IO ()
-- markupLn, markup, formatLn

-- Input ---------------------------------------------------------------------------------------------------------------------------------------------
