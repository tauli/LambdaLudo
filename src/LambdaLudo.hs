{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module LambdaLudo
  ( Square
  , Step
  , Handle
  , Config(..)
  , Color(..)
  , runGame
  , defaultHandler
  , nop
  , readColor
  , readFrameNr
  {-
  , findSpriteByXY
  , findSpriteByName
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
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State as S
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
  fst <$> S.runStateT loop (S.execState (runStep i >>= evalAction) s)

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

loop :: S.StateT (EngineState s) IO ()
loop = do
  events <- getEngineEvents
  case elem Quit events of
    True  -> return ()
    False -> do
      mapM_ (\e -> (gameHandler <$> S.get <*> pure e) >>= runStep) events
      (gameStepper <$> S.get) >>= runStep
      incFrame
      loop
{-
      s' = evalAction a s
      buildScene c s'' r
      present r
      waitForNext
-}

getEngineEvents :: S.StateT (EngineState s) IO [EngineEvent]
getEngineEvents = do
  e  <- map eventPayload <$> S.liftIO pollEvents
  ee <- mapM sdlToEE e
  return $ catMaybes ee

sdlToEE (KeyboardEvent (KeyboardEventData _ Pressed _ k)) = 
  return $ Just $ KeyPress $ keysymKeycode k
sdlToEE (MouseButtonEvent(MouseButtonEventData _ Pressed _ ButtonLeft _ p)) = do
  let (P (V2 x y)) = p
  s <- squareSize <$> S.get
  return $ Just $ MouseClick (div (fromEnum x) s, div (fromEnum y) s)
sdlToEE QuitEvent = return $ Just Quit
sdlToEE _ = return Nothing

runStep :: S.MonadState (EngineState s) m => Step s () -> m [Action]
runStep step = do
  st <- S.get
  let ((gs,as),r) = runRand 
                    (RWS.execRWST step st (gameState st)) 
                    (randomState st)
  S.put $ st {gameState = gs, randomState = r}
  return as

incFrame :: S.MonadState (EngineState s) m => m ()
incFrame = do
  s <- S.get
  let f = frame s
  S.put $ s {frame = succ f} 

{-

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
-}

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

evalAction :: [Action] -> S.State (EngineState s) ()
evalAction = mapM_ evalAction' where
  evalAction' (PaintSquare (x,y) c) = modSquare (x,y) (setColor c)
  evalAction' _ = undefined
{-
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
-}

modSquare :: (Int,Int) -> (Square -> Square) -> S.State (EngineState s) ()
modSquare (x,y) f = S.modify (\st -> st {board = modSquare' (board st)}) where 
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

defaultHandler :: Handle s ()
defaultHandler _ = nop

nop :: Step s ()
nop = return ()

findSquare :: (Int,Int) -> Board -> Maybe Square
findSquare (x,y) b = case filter matchSquare b of
  (s:_) -> Just s
  _     -> Nothing
  where
    matchSquare :: Square -> Bool
    matchSquare (Square x' y' _ _ _) = x == x' && y == y'

readColor :: RWS.MonadReader (EngineState s) m => (Int,Int) -> m Color
readColor c = do
  b <- RWS.reader board
  case findSquare c b of
    Just (Square _ _ _ _ (Color r g b)) -> return $ Color r g b
    _                                   -> return Transparent

readFrameNr :: RWS.MonadReader (EngineState s) m => m Int
readFrameNr = RWS.reader frame

{-
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

paintSquare  xy c         = RWS.tell [PaintSquare  xy c        ]
createSprite xy z   sName = RWS.tell [CreateSprite xy z   sName]
deleteSprite xy     sName = RWS.tell [DeleteSprite xy     sName]
moveSprite   xy xy' sName = RWS.tell [MoveSprite   xy xy' sName]

