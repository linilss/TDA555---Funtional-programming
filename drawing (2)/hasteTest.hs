import Haste
import Haste.Graphics.Canvas

import Pages

ball :: Double -> Picture ()
ball y = fill $ circle (100,y) 10

fall :: Canvas -> Elem -> Int -> IO ()
fall can boom v = do
    render can $ ball y
    if y < 600
      then setTimeout 20 $ fall can boom (v+1)
      else setProp boom "innerHTML" "!!! *** BOOM *** !!!"
  where
    v' = fromIntegral v
    y  = 0.03 * v'^2

main :: IO ()
main = do
    canvas <- mkCanvas 300 600
    boom   <- newElem "span"
    column documentBody [canvas,boom]
    setStyle boom "fontSize" "150%"
    Just can <- getCanvas canvas
    fall can boom 0
