module Main where 

import LambdaLudo

conf = Config 
  { stepper     = nop --step
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
initGame = setBackgroundColor $ Color 0 0 0

step :: Step () ()
step = do
  f <- readFrameNr
  if rem f 3 == 0 then mapM_ fade allSquares else return ()

allSquares :: [(Int,Int)]
allSquares = [(x,y)|x<-[0..9],y<-[0..9]]

fade :: (Int,Int) -> Step () ()
fade p = do
  c <- readColor p
  case c of 
    Transparent   -> return ()
    (Color r g b) -> paintSquare p $ Color (div r 2) (div g 2) (div b 2)

handle :: Handle () ()
handle (MouseHover p) = do
  val <- getRandomNumber (0,255)
  ch  <- getRandomNumber (0,2)
  let c = case ch of
            0 -> Color val (255 - val) 0
            1 -> Color 0 val (255 - val)
            2 -> Color (255 - val) 0 val
  paintSquare p c
handle _ = nop

