module Render where

import Logic

import Graphics.DrawingCombinators as D hiding (point)

import Control.Monad
import Data.Maybe
import Data.Monoid

import Control.Lens

import qualified Data.Vector as V

import System.IO.Unsafe

import Control.Monad.Trans.Writer

type Render a = Writer (Image Any) a

runRender :: Render a -> IO ()
runRender = D.render . execWriter

font :: Font
font = unsafePerformIO $ openFont "/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf"

holdWidth :: R
holdWidth = textWidth font "hold"

nextWidth :: R
nextWidth = textWidth font "next"

renderGame :: Gamestate -> Render ()
renderGame gs = do
    renderBoard . view tetBoard $ gs
    renderBoard . view board $ gs

    censor (tint $ Color 1.0 1.0 1.0 0.5) $ renderBoard . view tetBoard $ doQuickFall gs

    tell $ tint (Color 0.1 0.1 0.1 1.0) $ convexPoly [(0,0),(300,0),(300,600),(0,600)]

    tell $ translate (350 , 30) %% scale 10 (-10) %% translate (-0.5*holdWidth,0) %% text font "hold"
    case (view held gs) of
        Just t -> censor (((translate (350, 70)) %%) . ((scale 20 20) %%)) $ renderTetromino t
        _ -> return ()

    tell $ translate (350 , 130) %% scale 10 (-10) %% translate (-0.5*nextWidth,0) %% text font "next"
    let nexts = take 4 $ view bag gs `zip` [1..]
    forM_ nexts $ \(t, n) -> censor (((translate (350, 110+60*n)) %%) . ((scale 20 20) %%)) $ renderTetromino t

renderBoard :: Board -> Render ()
renderBoard (Board rows) = forM_ (zip [0..] $ V.toList rows) $ \(n, row) ->
    censor ((translate (0, 30*n + 15)) %%) $ renderRow row

renderRow :: Row -> Render ()
renderRow (Row cells) = forM_ (zip [0..] $ V.toList cells) $ \(n, cell) ->
    censor ((translate (30*n + 15, 0)) %%) $ renderCell cell

renderCell :: Cell -> Render ()
renderCell Empty = return ()
renderCell (Filled t) = tell $ tint (tetrominoColor t) $ scale 26 26 %% convexPoly [(-0.5,-0.5),(0.5,-0.5),(0.5,0.5),(-0.5,0.5)]

tetrominoColor :: Tetromino -> Color
tetrominoColor I = Color 0.0 1.0 1.0 1.0
tetrominoColor O = Color 1.0 1.0 0.0 1.0
tetrominoColor T = Color 1.0 0.0 1.0 1.0
tetrominoColor S = Color 0.0 1.0 0.0 1.0
tetrominoColor Z = Color 1.0 0.0 0.0 1.0
tetrominoColor J = Color 0.0 0.0 1.0 1.0
tetrominoColor L = Color 1.0 0.5 0.0 1.0

renderTetromino :: Tetromino -> Render ()
renderTetromino t = tell $ tint (tetrominoColor t) $ case t of
    I -> convexPoly [(-2,-0.5),(-2,0.5),(2,0.5),(2,-0.5)]
    O -> convexPoly [(-1,-1),(-1,1),(1,1),(1,-1)]
    T -> mconcat [
            convexPoly [(-0.5,-1),(-0.5,0),(0.5,0),(0.5,-1)]
          , convexPoly [(-1.5,0),(-1.5,1),(1.5,1),(1.5,0)]
          ]
    S -> mconcat [
            convexPoly [(-0.5,0),(1.5,0),(1.5,-1),(-0.5,-1)]
          , convexPoly [(-1.5,1),(0.5,1),(0.5,0),(-1.5,0)]
          ]
    Z -> mconcat [
            convexPoly [(-1.5,0),(0.5,0),(0.5,-1),(-1.5,-1)]
          , convexPoly [(-0.5,1),(1.5,1),(1.5,0),(-0.5,0)]
          ]
    J -> mconcat [
            convexPoly [(-1.5,0),(-0.5,0),(-0.5,-1),(-1.5,-1)]
          , convexPoly [(-1.5,1),(1.5,1),(1.5,0),(-1.5,0)]
          ]
    L -> mconcat [
            convexPoly [(1.5,0),(0.5,0),(0.5,-1),(1.5,-1)]
          , convexPoly [(-1.5,1),(1.5,1),(1.5,0),(-1.5,0)]
          ]
