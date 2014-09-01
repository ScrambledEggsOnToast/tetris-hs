{-# LANGUAGE RankNTypes, Arrows #-}
module Input where

import Prelude hiding ((.), id)
import Control.Category

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Graphics.UI.GLFW as G
import Control.Wire.Core
import FRP.Netwire
import Control.Wire.Unsafe.Event (Event(..))

type Input a = (HasTime t s, MonadIO m) => Wire s () (ReaderT Window m) () a

data Moving = ML | MR deriving Eq
data Rotating = CW | CCW deriving Eq

data InputData = InputData {
    inputdataMoving :: Event (Maybe Moving)
  , inputdataRotating :: Event (Maybe Rotating)
  , inputdataQuickFall :: Event ()
  , inputdataSlowFall :: Bool
  , inputdataHold :: Event ()
}

processInput :: (HasTime t s, MonadIO m) => Window -> Input a -> Wire s () m () a
processInput w i = mapWire (\r -> runReaderT r w) i

inputWire :: Input InputData
inputWire = InputData <$> movingWire <*> rotatingWire <*> quickFallWire <*> slowFallWire <*> holdWire

movingWire :: Input (Event (Maybe Moving))
movingWire =  
  let io = mkGen_ $ \_ -> do
        w <- ask
        left <- liftIO $ getKey w Key'Left
        right <- liftIO $ getKey w Key'Right
        return . Right $ case (left, right) of
            (KeyState'Pressed, _) -> Just ML
            (_, KeyState'Pressed) -> Just MR
            _ -> Nothing
  in (proc _ -> do
    mm <- io -< ()
    dmm <- delay Nothing <<< io -< ()
    let emm = if dmm /= mm then Event mm else NoEvent
    returnA -< emm) 

slowFallWire :: Input Bool
slowFallWire = mkGen_ $ \_ -> do
    w <- ask
    f <- liftIO $ getKey w Key'Down
    return . Right $ case f of
        KeyState'Released -> False
        _ -> True

quickFallWire :: Input (Event ())
quickFallWire = 
  let io = mkGen_ $ \_ -> do
        w <- ask
        d <- liftIO $ getKey w Key'Space
        return . Right $ case d of
            KeyState'Pressed -> True
            _ -> False
  in proc _ -> do
    mf <- io -< ()
    dmf <- delay False <<< io -< ()
    let emf = if dmf /= mf && mf /= False then Event () else NoEvent
    returnA -< emf

holdWire :: Input (Event ())
holdWire = 
  let io = mkGen_ $ \_ -> do
        w <- ask
        l <- liftIO $ getKey w Key'LeftShift
        r <- liftIO $ getKey w Key'RightShift
        return . Right $ case (l,r) of
            (KeyState'Pressed,_) -> True
            (_,KeyState'Pressed) -> True
            _ -> False
  in proc _ -> do
    mf <- io -< ()
    dmf <- delay False <<< io -< ()
    let emf = if dmf /= mf && mf /= False then Event () else NoEvent
    returnA -< emf

rotatingWire :: Input (Event (Maybe Rotating))
rotatingWire = 
  let io = mkGen_ $ \_ -> do
        w <- ask
        cw <- liftIO $ getKey w Key'Up
        return . Right $ case cw of
            KeyState'Pressed -> Just CW
            _ -> Nothing
  in proc _ -> do
    mr <- io -< ()
    dmr <- delay Nothing <<< io -< ()
    let emr = if dmr /= mr then Event mr else NoEvent
    returnA -< emr
