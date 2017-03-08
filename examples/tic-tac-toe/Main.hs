module Main where 

import LambdaLudo

conf = Config 
  { stepper     = nop
  , handler     = handle
  , initializer = initGame
  , memory      = PlayerX
  , assets      = ["Background.png", "X.png", "O.png"]
  , columns     = 3
  , rows        = 3
  , size        = 300
  }

data Player = PlayerX | PlayerO
  
main :: IO ()
main = runGame conf

initGame :: Step Player ()
initGame = setBackgroundImage "Background"

handle :: Handle Player ()
handle (MouseClick p) = do
  state <- get
  let spr = playerSprite state
  alreadyMarked <- marked p
  if alreadyMarked then nop else do
    createSprite p 1 spr
    put $ nextPlayer state
handle _ = nop

marked :: (Int,Int) -> Step Player Bool
marked p = findSpriteByXY p >>= (return . ( /= []))

playerSprite :: Player -> String
playerSprite PlayerX = "X"
playerSprite PlayerO = "O"

nextPlayer :: Player -> Player
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

