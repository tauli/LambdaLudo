module Main where

import LambdaLudo

conf = Config
  { stepper     = anim
  , handler     = handle
  , initializer = initBoard
  , memory      = ()
  , assets      =
    [ "Character1.png"
    , "Character2.png"
    , "Character4.png"
    , "Character7.png"
    , "Crate_Blue.png"
    , "CrateDark_Blue.png"
    , "EndPoint_Blue.png"
    , "Wall_Brown.png"
    ]
  , columns     = 19
  , rows        = 11
  , size        = 64
  }

main :: IO ()
main = runGame conf

initBoard :: Step () ()
initBoard = do
  setBackgroundColor (Color 128 128 128)
  mapM_ placeSprite $ addCoordinates puzzle

anim :: Step () ()
anim = nop

handle :: Handle () ()
handle (KeyPress KeycodeUp) = do
  ((x,y),name) <- findChar
  deleteSprite (x,y) name
  createSprite (x,y) 2 "Character7"
  moveChar (x,y) (x,y-1) (x,y-2) "Character7"
handle (KeyPress KeycodeDown) = do
  ((x,y),name) <- findChar
  deleteSprite (x,y) name
  createSprite (x,y) 2 "Character4"
  moveChar (x,y) (x,y+1) (x,y+2) "Character4"
handle (KeyPress KeycodeLeft) = do
  ((x,y),name) <- findChar
  deleteSprite (x,y) name
  createSprite (x,y) 2 "Character1"
  moveChar (x,y) (x-1,y) (x-2,y) "Character1"
handle (KeyPress KeycodeRight) = do
  ((x,y),name) <- findChar
  deleteSprite (x,y) name
  createSprite (x,y) 2 "Character2"
  moveChar (x,y) (x+1,y) (x+2,y) "Character2"
handle _ = nop

moveChar :: (Int,Int) -> (Int,Int) -> (Int,Int) -> String -> Step () ()
moveChar xy1 xy2 xy3 name = do
  n <- findSpriteByXY xy2
  if elem "Wall_Brown" n
    then nop
    else case filter isBox n of
      []         -> moveSprite xy1 xy2 name
      (bName:_)  -> do
        n' <- findSpriteByXY xy3
        let b = filter isBox n'
        if elem "Wall_Brown" n' || b /= []
          then nop
          else do
            moveSprite xy1 xy2 name
            deleteSprite xy2 bName
            let newCrate = if elem "EndPoint_Blue" n'
                             then "CrateDark_Blue"
                             else "Crate_Blue"
            createSprite xy3 1 newCrate

findChar :: Step () ((Int,Int),String)
findChar = do
  result <- mapM findSpriteWithName charNames
  return $ head $ concat result

findSpriteWithName :: String -> Step () [((Int,Int),String)]
findSpriteWithName n = do
  result <- findSpriteByName n
  return $ zip result $ repeat n

charNames :: [String]
charNames = [ "Character1" , "Character2"
            , "Character4" , "Character7"]

isBox :: String -> Bool
isBox s = s == "Crate_Blue" || s == "CrateDark_Blue"

addCoordinates :: [String] -> [((Int,Int),Char)]
addCoordinates p = concatMap addMore $ zip [0..] p where
  addMore (y,s) = map (\(x,c) -> ((x,y),c) ) $ zip [0..] s

placeSprite :: ((Int,Int),Char) -> Step () ()
placeSprite (xy,' ') = nop
placeSprite (xy,'#') = createSprite xy 1 "Wall_Brown"
placeSprite (xy,'$') = createSprite xy 1 "Crate_Blue"
placeSprite (xy,'@') = createSprite xy 2 "Character4"
placeSprite (xy,'.') = createSprite xy 0 "EndPoint_Blue"

puzzle :: [String]
puzzle =
  [ "    #####          "
  , "    #   #          "
  , "    #$  #          "
  , "  ###  $##         "
  , "  #  $ $ #         "
  , "### # ## #   ######"
  , "#   # ## #####  ..#"
  , "# $  $          ..#"
  , "##### ### #@##  ..#"
  , "    #     #########"
  , "    #######        "
  ]
