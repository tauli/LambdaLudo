module Main where

import LambdaLudo

conf = Config
  { stepper     = nop
  , handler     = handle
  , initializer = initGame
  , memory      = PlayerX
  , assets      = ["Background.png", "X.png", "O.png", "Marker.png"]
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
  player <- get
  let image = playerSprite player
  alreadyMarked <- marked p
  if alreadyMarked then nop else do
    createSprite p 1 image
    won <- hasWon p
    if won == [] then nop else do
      mapM_ (\x -> createSprite x 2 "Marker") won
      changeHandler triggerNewGame
    draw <- checkDraw p
    if draw then changeHandler triggerNewGame else nop
    put (nextPlayer player)
handle _ = nop

marked :: (Int,Int) -> Step Player Bool
marked square = do
  sprites <- findSpriteByXY square
  return (sprites /= [])

playerSprite :: Player -> String
playerSprite PlayerX = "X"
playerSprite PlayerO = "O"

nextPlayer :: Player -> Player
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

possibleWins :: [[(Int,Int)]]
possibleWins =
  [ [(0,0),(1,0),(2,0)], [(0,1),(1,1),(2,1)], [(0,2),(1,2),(2,2)]
  , [(0,0),(0,1),(0,2)], [(1,0),(1,1),(1,2)], [(2,0),(2,1),(2,2)]
  , [(0,0),(1,1),(2,2)], [(2,0),(1,1),(0,2)]
  ]

checkWin :: [(Int,Int)] -> Step Player [(Int,Int)]
checkWin w = do
  marks <- mapM findSpriteByXY w
  player <- get
  let p = playerSprite player
  return (if marks == [[p],[p]] then w else [])

hasWon :: (Int,Int) -> Step Player [(Int,Int)]
hasWon p = do
  w <- mapM (checkWin . filter (/= p)) possibleWins
  let wins = filter (/= []) w
  return (if wins == [] then [] else p : head wins)

allSquares :: [(Int,Int)]
allSquares = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)]

removeAllSprites :: (Int,Int) -> Step Player ()
removeAllSprites sq = do
  s <- findSpriteByXY sq
  mapM_ (deleteSprite sq) s

resetGame :: Step Player ()
resetGame = do
  mapM removeAllSprites allSquares
  changeHandler handle
  put PlayerX

triggerNewGame :: Handle Player ()
triggerNewGame (MouseClick _) = resetGame
triggerNewGame _ = nop

checkDraw :: (Int,Int) -> Step Player Bool
checkDraw p = do
  sq <- mapM findSpriteByXY (filter (/= p) allSquares)
  return (filter (== []) sq == [])
