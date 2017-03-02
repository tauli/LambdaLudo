{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module LambdaLudo
  ( Square
  , Step
  , Handle
  , Config(..)
  , Color(..)
  , runGame
  {-
  , defaultHandler
  , nop
  , readColor
  , findSpriteByXY
  , findSpriteByName
  , readFrameNr
  -}
  , paintSquare
  , createSprite
  , deleteSprite
  , moveSprite
  , module SDL.Input.Keyboard.Codes
  ) where

import LambdaLudo.Types

import SDL
import SDL.Input.Keyboard.Codes

import Control.Concurrent (threadDelay)
import Control.Monad.Random (runRand,getRandomR)
import Control.Monad.RWS
import Control.Monad.State
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Text (unpack)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import GHC.Exts (sortWith)
import System.Random (getStdGen)


runGame :: Config a -> IO ()
runGame c@(Config _ _ i _ a col row size) = do
  initializeAll
  w <- createWindow "Game" $ defaultWindow
    {windowInitialSize = V2 (toEnum $ col * size) (toEnum $ row * size)}
  r <- createRenderer w (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  s <- initState c r
  fst <$> runStateT loop (execState (runStep i >>= evalAction) s)

initState :: Config s -> Renderer -> IO (EngineState s)
initState conf r = do
  texture' <- loadTexture r (assets conf)
  randomState' <- getStdGen
  let frame'       = 0
      sprite'      = []
      boardWidth'  = columns conf
      boardHeight' = rows conf
      squareSize'  = size conf
      board'       = [
        Square x y "" 0 Transparent 
          | x <- [0..boardWidth'  - 1]
          , y <- [0..boardHeight' - 1]
        ] 
      gameStepper' = stepper conf
      gameHandler' = handler conf
      gameState'   = memory conf
  return $ EngineState
    frame' randomState' texture' sprite'
    board' boardWidth' boardHeight' squareSize'
    gameStepper' gameHandler' gameState'

loadTexture :: Renderer -> [FilePath] -> IO [(String,Texture)]
loadTexture r fs = zip tName <$> mapM loadTexture' fs where
  loadTexture' f = do
    bmp  <- loadBMP $ "img/" ++ f
    createTextureFromSurface r bmp <* freeSurface bmp
  tName = map (takeWhile (/= '.')) fs

loop :: StateT (EngineState s) IO ()
loop = undefined
{-
loop = do
  let f = frame s
      a = runStep s (stepper (config s))
      s' = evalAction a s
  events <- fmap (catMaybes . (map eventToKeycode)) pollEvents
  case elem KeycodeQ events of 
    True  -> return ()
    False -> do
      let s'' = handleKeys c s' events
      buildScene c s'' r
      present r
      waitForNext
      loop
-}

runStep :: Step s () -> State (EngineState s) [Action]
runStep step = do
  st <- Control.Monad.State.get
  let ((gs,as),r) = runRand (execRWST step st (gameState st)) (randomState st)
  put $ st {gameState = gs, randomState = r}
  return as

{-
eventToKeycode :: Event -> Maybe Keycode
eventToKeycode e = case eventPayload e of
  KeyboardEvent keyboardEvent ->
    if   keyboardEventKeyMotion keyboardEvent == Pressed 
    then Just $ keysymKeycode $ keyboardEventKeysym keyboardEvent
    else Nothing
  _ -> Nothing

handleKeys :: Config -> EngineState -> [Keycode] -> EngineState
handleKeys _ s []     = s
handleKeys c s (e:es) = 
  handleKeys c (evalAction (runStep s (handler c $ e)) s) es
  

buildScene :: Config -> EngineState -> Renderer -> IO ()
buildScene conf (EngineState b _ t s) r = do
  let bg = transToBlack $ bgColor conf
  rendererDrawColor r $= mkColor bg
  clear r
  mapM_ (drawSquare conf r) b
  mapM_ (drawSprite conf r) $ sortWith twoOfFour s where
    twoOfFour (_,x,_,_) = x

drawSquare :: Config -> Renderer -> Square -> IO ()
drawSquare _ _ (Square _ _ _ _ Transparent) = return ()
drawSquare conf r s@(Square x y _ _ c) = do
  rendererDrawColor r $= mkColor c
  fillRect r $ Just $ mkRect x y (size conf)

drawSprite :: Config -> Renderer -> Sprite -> IO ()
drawSprite conf r ((x,y),z,_,t) =
  copy r t Nothing (Just $ mkRect x y (size conf))

mkRect :: Int -> Int -> Int -> Rectangle CInt
mkRect x y s = Rectangle (P $ V2 x' y') (V2 s' s') where
  x' = toEnum $ x * s
  y' = toEnum $ y * s
  s' = toEnum s

mkColor :: Color -> V4 Word8
mkColor Transparent = V4 1 1 1 0
mkColor (Color r g b) = V4 (toEnum r) (toEnum g) (toEnum b) 255

transToBlack :: Color -> Color
transToBlack Transparent = Color 0 0 0
transToBlack c           = c
-}

evalAction :: [Action] -> State (EngineState s) ()
evalAction = undefined
{-
evalAction a s = foldl evalAction' s a where
  evalAction' s (PaintSquare (x,y) c) = modSquare s (x,y) (setColor c)
  evalAction' s (CreateSprite xy z name) = case lookup name (texture s) of
    Nothing -> error ("texture: " ++ name ++ " not found")
    Just t  -> s {sprite = (xy,z,name,t) : sprite s}
  evalAction' s (DeleteSprite (x,y)         name) = 
    s {sprite = filter (not . isSprite (x,y) name) $ sprite s}
    where 
        isSprite :: (Int,Int) -> String -> Sprite -> Bool
        isSprite (x',y') name' ((x,y),_,name,t) = 
          x == x' && y == y' && name == name'
  evalAction' s (MoveSprite   (x,y) (x',y') name) = 
    let (found,rest) = partition (isSprite (x,y) name) $ sprite s
    in  s {sprite = map (\(_,z,n,t) -> ((x',y'),z,n,t)) found ++ rest}
    where 
        isSprite :: (Int,Int) -> String -> Sprite -> Bool
        isSprite (x',y') name' ((x,y),_,name,t) = 
          x == x' && y == y' && name == name'

modSquare :: EngineState -> (Int,Int) -> (Square -> Square) -> EngineState
modSquare state (x,y) f = state {board = modSquare' (board state)} where
  modSquare' (s@(Square x' y' _ _ _):ss) = if x == x' && y == y'
    then f s : ss
    else s   : modSquare' ss
  modSquare' [] = []

setColor :: Color -> Square -> Square
setColor c s = s {color = c}

fps :: Integral a => a
fps = 30

frameTime :: Float
frameTime = 1000 / fromIntegral fps

waitForNext :: IO ()
waitForNext = do
  t <- ticks
  let f = (fromIntegral $ floor (fromIntegral t / frameTime)) * frameTime
      w = round $ frameTime - (fromIntegral t - f)
      w' = if w == 0 then 16 else w
  threadDelay $ 1000 * w


defaultHandler :: Handle ()
defaultHandler _ = nop

nop :: Step ()
nop = tell []

findSquare :: (Int,Int) -> Board -> Maybe Square
findSquare (x,y) b = case filter matchSquare b of
  (s:_) -> Just s
  _     -> Nothing
  where
    matchSquare :: Square -> Bool
    matchSquare (Square x' y' _ _ _) = x == x' && y == y'

readColor :: MonadReader EngineState m => (Int,Int) -> m Color
readColor c = do
  b <- reader board
  case findSquare c b of
    Just (Square _ _ _ _ (Color r g b)) -> return $ Color r g b
    _                                   -> return Transparent

readFrameNr :: MonadReader EngineState m => m Int
readFrameNr = reader frame

findSpriteByXY :: MonadReader EngineState m => (Int,Int) -> m [String]
findSpriteByXY (x,y) = do
  s <- reader sprite
  return $ map (\(_,_,name,_) -> name) $ filter (isSprite (x,y)) s where
    isSprite :: (Int,Int) -> Sprite -> Bool
    isSprite (x',y') ((x,y),_,name,t) = x == x' && y == y'

findSpriteByName :: MonadReader EngineState m => String -> m [(Int,Int)]
findSpriteByName name = do
  s <- reader sprite
  return $ map (\(xy,_,_,_) -> xy) $ filter (isSprite name) s where
    isSprite :: String -> Sprite -> Bool
    isSprite name' ((x,y),_,name,t) = name == name'
-}

paintSquare  xy c         = tell [PaintSquare  xy c        ]
createSprite xy z   sName = tell [CreateSprite xy z   sName]
deleteSprite xy     sName = tell [DeleteSprite xy     sName]
moveSprite   xy xy' sName = tell [MoveSprite   xy xy' sName]

