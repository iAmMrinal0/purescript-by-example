module Main where

import Control.Monad.Eff.Console
import Prelude

import Math (pi, sqrt)

diagonal w h = sqrt(w*w + h*h)

circleArea r = pi * r * r

main = do
       logShow (circleArea 3.0)
       logShow (diagonal 3.0 3.0)
