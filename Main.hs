module Main where

import Logic
import Input
import Render

import Control.Monad.Trans.State
import Control.Concurrent (threadDelay)
import Control.Monad as M
import Control.Exception
import Control.Monad.IO.Class

import qualified Graphics.UI.GLFW as G
import Graphics.UI.GLFW as G hiding (init)
import Graphics.Rendering.OpenGL as GL

import Graphics.DrawingCombinators as D

import Graphics.GLUtil

import FRP.Netwire as W
import Control.Wire.Core

import Data.IORef

import System.Random

main = do
    True <- G.init

    let fps = 60

    g <- newStdGen

    Just w <- createWindow 420 620 "tetris-hs" Nothing Nothing
    makeContextCurrent (Just w)

    setWindowSizeCallback w $ Just (\_ width height -> resize (width, height))

    resize (420,620)

    done <- newIORef False

    let loop wire s = do
            (ds, s') <- stepSession s
            (mgs, wire') <- liftIO $ stepWire wire ds (Right ())

            Just now <- getTime

            clearColor $= Color4 0 0 0 1

            either (const $ writeIORef done True) (\gs -> do
                clear [ColorBuffer, DepthBuffer]

                loadIdentity

                runRender $ renderGame gs

                flush

                swapBuffers w

                ) mgs

            pollEvents

            Just frameLeft <- fmap ((recip fps) + now -) <$> getTime

            esc <- (== KeyState'Pressed) <$> getKey w Key'Escape
            d <- readIORef done
            M.unless (esc || d) $ do
                M.when (frameLeft > 0) $
                    threadDelay (truncate $ 1000000 * frameLeft)

                loop wire' s'

    finally (loop (processInput w inputWire >>> gameLogic (newGame g)) clockSession_) (quit w)

resize (w, h) = do
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    matrixMode $= Projection
    loadIdentity
    ortho (-10) 410 610 (-10) 0 1
    matrixMode $= Modelview 0

quit w = do
    destroyWindow w
    terminate
