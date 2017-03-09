module Main where 

import LambdaLudo

conf = Config 
  { stepper     = nop
  , handler     = handle
  , initializer = initGame
  , memory      = Black
  , assets      = ["Board.png", "BlackStone.png", "WhiteStone.png"]
  , columns     = 19
  , rows        = 19
  , size        = 64
  }

data Player = Black | White deriving (Eq, Show)
  
main :: IO ()
main = runGame conf

initGame :: Step Player ()
initGame = setBackgroundImage "Board"

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
playerSprite Black = "BlackStone"
playerSprite White = "WhiteStone"

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black
