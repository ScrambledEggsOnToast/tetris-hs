{-# LANGUAGE RankNTypes, Arrows, TemplateHaskell, FlexibleContexts #-}
module Logic where

import Prelude hiding ((.), id)
import Control.Category

import qualified Data.Vector as V
import Data.List (intercalate, transpose)

import FRP.Netwire
import Control.Wire as W
import Control.Wire.Unsafe.Event (Event(NoEvent))
import Control.Applicative
import Data.Monoid
import Data.Maybe

import Control.Lens

import System.Random

import Debug.Trace

import Input
import Util

data Tetromino = I | O | J | L | Z | S | T deriving (Show, Read, Eq, Enum)

instance Random Tetromino where
    random = randomR (I,T)
    randomR (a,b) g = first toEnum $ randomR (fromEnum a, fromEnum b) g

data Cell = Filled Tetromino | Empty deriving (Show, Read, Eq)

data Row = Row { rowCells :: V.Vector Cell } deriving (Show, Read, Eq)

data Board = Board { boardRows :: V.Vector Row } deriving (Show, Read, Eq)

type Grid = Board

type Logic a b = (HasTime NominalDiffTime s, Monad m) => Wire s () m a b

data Gamestate = Gamestate {
    _board :: Board
  , _tetBoard :: Board
  , _tet :: Tetromino
  , _pos :: (Int, Int, Int)
  , _bag :: [Tetromino]
  , _gen :: StdGen
  , _timeLeft :: NominalDiffTime
  , _held :: Maybe Tetromino
  , _haveHeld :: Bool
  , _score :: Int
  , _level :: Int
} deriving Show
makeLenses ''Gamestate

setE a = arr (fmap $ const a)

dropTime :: Int -> NominalDiffTime
dropTime n = 0.06 * fromIntegral (16 - min 15 n)

iPos :: Tetromino -> (Int, Int, Int)
iPos O = (4,0,0)
iPos _ = (3,0,0)

randomBag :: StdGen -> ([Tetromino], StdGen)
randomBag g = shuffle [I,O,J,L,Z,S,T] g

newGame :: StdGen -> Gamestate 
newGame g = let
    (iBag, g') = randomBag g
    first = head iBag
  in 
    Gamestate emptyBoard (inBoard first $ iPos first) first (iPos first) (tail iBag) g' (dropTime 1) Nothing False 0 1

checkRows :: Gamestate -> Gamestate
checkRows = over board $ check 19
  where
    check (-1) b = b
    check n b@(Board rows) = if rowFull (rows V.! n) then check n (deleteRow n b) else check (n-1) b

tetPos :: Functor f => ((Tetromino, (Int, Int, Int)) -> f (Tetromino, (Int, Int, Int))) -> Gamestate -> f Gamestate
tetPos = lens ((,) <$> view tet <*> view pos) (\gs (t,p) -> gs & (tetBoard .~ (inBoard t p)) . (tet .~ t) . (pos .~ p))

doHold :: Gamestate -> Gamestate
doHold gs
    | view haveHeld gs = gs
    | otherwise = let 
        gs' = fillOutBag gs & (held .~ (Just $ gs ^. tet))
                            . (haveHeld .~ True)
                            . (timeLeft .~ dropTime (gs ^. level))
        in case (view held gs) of
            Just h -> gs' & (tetPos .~ (h, iPos h))
            Nothing -> let n = head $ gs' ^. bag
              in gs' & (tetPos .~ (n, iPos n))
                     . (over bag tail)
            
fillOutBag :: Gamestate -> Gamestate
fillOutBag gs =
  let
    (newBag, g) = if 4 >= (length $ view bag gs) then randomBag (view gen gs) else ([], view gen gs)
    bag' = (view bag gs) ++ newBag
  in 
    set bag bag' . set gen g $ gs

doDrop :: Gamestate -> (Gamestate, Bool)
doDrop gs = 
  let
    mDrop = moveDown (view tetBoard gs) (view board gs)
    posMove = \(x,y,r) -> (x,y+1,r)
    bag' = view bag $ fillOutBag gs
    n = head bag'
    nextGs = checkRows (gs & (board .~ flatten (gs ^. board) (gs ^. tetBoard))
                           . (tetPos .~ (n, iPos n))
                           . (bag .~ tail bag')
                           . (timeLeft .~ dropTime (gs ^. level))
                           . (haveHeld .~ False))
  in 
    maybe (nextGs, False) (\d -> (over pos posMove . set tetBoard d $ gs, True)) mDrop

doQuickFall :: Gamestate -> Gamestate
doQuickFall gs = let (dgs, dropped) = doDrop gs in if dropped then doQuickFall dgs else gs

doMove :: Moving -> Gamestate -> Gamestate
doMove m gs = 
  let
    mMove = move m (view tetBoard gs) (view board gs)
    posMove = case m of
        ML -> \(x,y,r) -> (x-1,y,r)
        MR -> \(x,y,r) -> (x+1,y,r)
  in
    maybe gs (\d -> over pos posMove . set tetBoard d $ gs) mMove

doRotate :: Rotating -> Gamestate -> Gamestate
doRotate rd gs = 
  let
    (x,y,r) = view pos gs
    r' = (`mod` 4) $ r + case rd of
        CW -> 1
        CCW -> (-1)
    rotated = inBoard (view tet gs) (x,y,r')
    b = view board gs
    c = filledCount $ flatten rotated b
    mRotate = if c - 4 == filledCount b then Just rotated else Nothing
  in
    maybe gs (\d -> set pos (x,y,r') . set tetBoard d $ gs) mRotate
    
gameLogic :: Gamestate -> Logic InputData Gamestate
gameLogic gs = switch (go gs)
  where 
    go gs = proc inputData -> do
        slowDropping <- arr inputdataSlowFall -< inputData
        let dropT = if slowDropping then 0.05 else dropTime $ gs ^. level
        (tl,de) <- dropEvent (view timeLeft gs) -< ()
        let dropE = fmap (const (gameLogic (fst $ doDrop (set timeLeft dropT gs)))) de
        me <- moveEvent -< inputData
        moveE <- delay NoEvent -< fmap (\m -> gameLogic (doMove m (set timeLeft tl gs))) me
        re <- rotateEvent -< inputData
        rotateE <- delay NoEvent -< fmap (\r -> gameLogic (doRotate r (set timeLeft tl gs))) re
        qfe <- quickFallEvent -< inputData
        quickFallE <- delay NoEvent -< fmap (const (gameLogic (set timeLeft dropT . doQuickFall $ gs))) qfe 
        he <- holdEvent -< inputData
        holdE <- delay NoEvent -< fmap (const (gameLogic (doHold gs))) he
        returnA -< (gs, foldl mergeL NoEvent [holdE, quickFallE, rotateE, moveE, dropE])

dropEvent :: NominalDiffTime -> Logic () (NominalDiffTime, Event ())
dropEvent t = proc _ -> do
    e <- W.at t -< ()
    tl <- arr ((+t) . negate) <<< timeF -< ()
    returnA -< (tl,e)

moveEvent :: Logic InputData (Event Moving)
moveEvent = arr (fmap fromJust) <<< filterE isJust <<< arr inputdataMoving

holdEvent :: Logic InputData (Event ())
holdEvent = arr inputdataHold

quickFallEvent :: Logic InputData (Event ())
quickFallEvent = arr inputdataQuickFall

rotateEvent :: Logic InputData (Event Rotating)
rotateEvent = arr (fmap fromJust) <<< filterE isJust <<< arr inputdataRotating

filledCount :: Board -> Int
filledCount (Board rows) = sum . map (V.length . V.filter isFilled . rowCells) . V.toList $ rows

flatten :: Board -> Board -> Board
flatten a b = 
      Board . V.fromList 
    . map (Row . V.fromList)
    $ zipWith (zipWith (\x y -> case (x,y) of 
          (Filled t,_) -> Filled t
          (_,Filled t) -> Filled t
          _ -> Empty)) (ll a) (ll b)
  where
    ll = V.toList . fmap (V.toList . rowCells) . boardRows

moveDown :: Grid -> Board -> Maybe Grid
moveDown t@(Board tRows) b = 
  let
    movedDown = Board $ (V.singleton emptyRow) V.++ (V.init tRows)
    c = filledCount $ flatten movedDown b
  in
    if c - 4 == filledCount b then Just movedDown else Nothing

move :: Moving -> Grid -> Board -> Maybe Grid
move m t@(Board tRows) b = 
  let
    moved = Board $ flip V.map tRows $ case m of
        ML -> \(Row r) -> Row $ V.tail r V.++ V.singleton Empty
        MR -> \(Row r) -> Row $ V.singleton Empty V.++ V.init r
    c = filledCount $ flatten moved b
  in
    if c - 4 == filledCount b then Just moved else Nothing

inBoard :: Tetromino -> (Int, Int, Int) -> Board
inBoard t (x,y,r) = Board rows
  where 
    (Board grid) = (!! r) . iterate rotateGridCW . tetrominoGrid $ t
    gridRows = fmap (\(Row cs) -> Row $ V.take 10 $ mconcat 
        [V.replicate x Empty, cs, V.replicate 10 Empty]) $ grid
    preRows = V.replicate y emptyRow
    postRows = V.replicate 20 emptyRow
    rows = V.take 20 $ mconcat $ [preRows, gridRows, postRows]
    
tetrominoGrid :: Tetromino -> Board
tetrominoGrid I = Board $ V.fromList 
    [ Row $ V.replicate 4 Empty
    , Row $ V.replicate 4 $ Filled I
    , Row $ V.replicate 4 Empty
    , Row $ V.replicate 4 Empty
    ]
tetrominoGrid O = Board $ V.fromList
    [ Row $ V.replicate 2 $ Filled O
    , Row $ V.replicate 2 $ Filled O
    ]
tetrominoGrid L = Board $ V.fromList
    [ Row $ V.fromList [Empty, Empty, Filled L]
    , Row $ V.replicate 3 $ Filled L
    , Row $ V.replicate 3 Empty
    ]
tetrominoGrid J = Board $ V.fromList
    [ Row $ V.fromList [Filled J, Empty, Empty]
    , Row $ V.replicate 3 $ Filled J
    , Row $ V.replicate 3 Empty
    ]
tetrominoGrid Z = Board $ V.fromList
    [ Row $ V.fromList [Filled Z, Filled Z, Empty]
    , Row $ V.fromList [Empty, Filled Z, Filled Z]
    , Row $ V.replicate 3 Empty
    ]
tetrominoGrid S = Board $ V.fromList
    [ Row $ V.fromList [Empty, Filled S, Filled S]
    , Row $ V.fromList [Filled S, Filled S, Empty]
    , Row $ V.replicate 3 Empty
    ]
tetrominoGrid T = Board $ V.fromList
    [ Row $ V.fromList [Empty, Filled T, Empty]
    , Row $ V.fromList [Filled T, Filled T, Filled T]
    , Row $ V.replicate 3 Empty
    ]

rotateGridCCW :: Grid -> Grid
rotateGridCCW grid = Board . V.fromList . map (Row . V.fromList) $ rotated
    where
        listOfLists = V.toList . fmap (V.toList . rowCells) . boardRows $ grid
        rotated = transpose . map reverse $ listOfLists

rotateGridCW :: Grid -> Grid
rotateGridCW grid = Board . V.fromList . map (Row . V.fromList) $ rotated
    where
        listOfLists = V.toList . fmap (V.toList . rowCells) . boardRows $ grid
        rotated = map reverse . transpose $ listOfLists

emptyRow :: Row
emptyRow = Row $ V.replicate 10 Empty

emptyBoard :: Board
emptyBoard = Board $ V.replicate 20 emptyRow

deleteRow :: Int -> Board -> Board
deleteRow n brd = Board $ (V.singleton emptyRow) V.++ (V.ifilter (\i _ -> i/=n) . boardRows $ brd)

rowFull :: Row -> Bool
rowFull = V.all isFilled . rowCells

isFilled :: Cell -> Bool
isFilled c = case c of
    Filled _ -> True
    _ -> False
