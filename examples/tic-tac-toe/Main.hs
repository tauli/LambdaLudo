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
    won <- hasWon p
    if won then changeStepper (winanim spr) >> changeHandler (triggerNewGame)
           else put $ nextPlayer state
handle _ = nop

winanim :: String -> Step Player ()
winanim player = do
  fn <- readFrameNr
  if mod fn 10 /= 0 then return () else do 
    sq <- findFirstNot player
    case sq of
      Nothing  -> return ()
      Just sq' -> removeAllSprites sq' >> createSprite sq' 1 player

findFirstNot :: String -> Step a (Maybe (Int,Int))
findFirstNot player = do
  let allSqares = (,) <$> [0..2] <*> [0..2] 
  allSprites <- zip allSqares <$> mapM findSpriteByXY allSqares
  let allNonPlayerSquares = filter (not. elem player . snd) allSprites
  case allNonPlayerSquares of
    (s:_) -> return $ Just $ fst s
    []    -> return Nothing

triggerNewGame :: Handle Player ()
triggerNewGame (MouseClick _) = resetGame
triggerNewGame (KeyPress   _) = resetGame
triggerNewGame _ = nop

resetGame :: Step Player ()
resetGame = do
  mapM removeAllSprites $ (,) <$> [0..2] <*> [0..2] 
  changeStepper nop
  changeHandler handle
  put PlayerX

removeAllSprites :: (Int,Int) -> Step Player ()
removeAllSprites sq = findSpriteByXY sq >>= mapM_ (deleteSprite sq)

marked :: (Int,Int) -> Step Player Bool
marked p = findSpriteByXY p >>= (return . ( /= []))

playerSprite :: Player -> String
playerSprite PlayerX = "X"
playerSprite PlayerO = "O"

nextPlayer :: Player -> Player
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

hasWon :: (Int,Int) -> Step Player Bool
hasWon p = do
  player <- playerSprite <$> get
  let lastPlay  = filter (elem p) possibleWins
      lastPlay' = map (filter (not . (== p))) lastPlay
  any id <$> mapM (checkWin player) lastPlay'

checkWin :: String -> [(Int,Int)] -> Step a Bool
checkWin player ps = all id <$> mapM (\p -> elem player <$> findSpriteByXY p) ps

possibleWins :: [[(Int,Int)]]
possibleWins = 
  [ [(0,0),(1,0),(2,0)], [(0,1),(1,1),(2,1)], [(0,2),(1,2),(2,2)]
  , [(0,0),(0,1),(0,2)], [(1,0),(1,1),(1,2)], [(2,0),(2,1),(2,2)]
  , [(0,0),(1,1),(2,2)], [(2,0),(1,1),(0,2)]
  ]
