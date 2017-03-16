module Main where

import LambdaLudo

conf = Config
  { stepper     = step
  , handler     = handle
  , initializer = initGame
  , memory      = ()
  , assets      = []
  , columns     = 10
  , rows        = 10
  , size        = 100
  }

main :: IO ()
main = runGame conf

initGame :: Step () ()
initGame = nop

step :: Step () ()
step = do
  f <- readFrameNr
  if rem f 5 == 0
    then mapM_ fade allSquares
    else nop

handle :: Handle () ()
handle (MouseHover p) = do
  randColor <- getRandomNumber (0,255)
  randChan  <- getRandomNumber (0,2)
  case randChan of
    0 -> paintSquare p (Color randColor (255 - randColor) 0)
    1 -> paintSquare p (Color 0 randColor (255 - randColor))
    2 -> paintSquare p (Color (255 - randColor) 0 randColor)
handle _ = nop

fadeColor :: Color -> Color
fadeColor (Color r g b) = Color (div r 2) (div g 2) (div b 2)
fadeColor Transparent   = Transparent

fade :: (Int,Int) -> Step () ()
fade p = do
  c <- readColor p
  paintSquare p (fadeColor c)

allSquaresRow :: Int -> [(Int,Int)]
allSquaresRow y = map (\x -> (x,y)) [0..(columns conf - 1)]

allSquares :: [(Int,Int)]
allSquares = concat (map allSquaresRow [0..(rows conf - 1)])
