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
  , findSpriteByXY
  , findSpriteByName
  , paintSquare
  , createSprite
  , deleteSprite
  , moveSprite
  , module SDL.Input.Keyboard.Codes
  ) where

import LambdaLudo.EDSL
import LambdaLudo.Types

import SDL
import SDL.Image (loadTexture)
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
  fst <$> S.runStateT (loop r) (S.execState (runStep i) s)

initState :: Config s -> Renderer -> IO (EngineState s)
initState conf r = do
  let tName = map (takeWhile (/= '.')) (assets conf)
  texture' <- zip tName <$> mapM (loadTexture r) (assets conf)
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
      gameBg'      = BgColor $ Color 0 0 0
  return $ EngineState
    frame' randomState' texture' sprite'
    board' boardWidth' boardHeight' squareSize'
    gameStepper' gameHandler' gameState' gameBg'

{-
loadTexture :: Renderer -> [FilePath] -> IO [(String,Texture)]
loadTexture r fs = zip tName <$> mapM loadTexture' fs where
  loadTexture' f = do
    bmp  <- loadBMP $ "img/" ++ f
    createTextureFromSurface r bmp <* freeSurface bmp
  tName = map (takeWhile (/= '.')) fs
-}

loop :: Renderer -> S.StateT (EngineState s) IO ()
loop r = do
  events <- getEngineEvents
  case elem Quit events of
    True  -> return ()
    False -> do
      mapM_ (\e -> (gameHandler <$> S.get <*> pure e) >>= runStep) events
      (gameStepper <$> S.get) >>= runStep
      incFrame
      buildScene r
      present r
      S.liftIO $ waitForNext
      loop r

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
sdlToEE (MouseMotionEvent(MouseMotionEventData _ _ _ p _)) = do
  let (P (V2 x y)) = p
  s <- squareSize <$> S.get
  return $ Just $ MouseHover (div (fromEnum x) s, div (fromEnum y) s)
sdlToEE QuitEvent = return $ Just Quit
sdlToEE _ = return Nothing

runStep :: S.MonadState (EngineState s) m => Step s () -> m ()
runStep step = do
  st <- S.get
  let ((gs,as),r) = runRand 
                    (RWS.execRWST step st (gameState st)) 
                    (randomState st)
  S.put $ st {gameState = gs, randomState = r}
  mapM_ (\a -> S.modify $ evalAction a) as

incFrame :: S.MonadState (EngineState s) m => m ()
incFrame = do
  s <- S.get
  let f = frame s
  S.put $ s {frame = succ f} 

buildScene :: Renderer -> S.StateT (EngineState s) IO ()
buildScene r = do
  clear r
  size <- squareSize  <$> S.get
  b    <- board       <$> S.get
  s    <- sprite      <$> S.get
  w    <- boardWidth  <$> S.get
  h    <- boardHeight <$> S.get
  bg   <- gameBg      <$> S.get
  S.liftIO $ do
    drawBackground size w h r bg 
    mapM_ (drawSquare size r) b
    mapM_ (drawSprite size r) $ sortWith (\(_,x,_,_) -> x) s

drawBackground :: Int -> Int -> Int -> Renderer -> GameBg -> IO ()
drawBackground s w h r (BgTexture tx) = 
  copy r tx Nothing $ Just $ mkRect 0 0 (w * s) (h * s)
drawBackground s w h r (BgColor c) = do
  rendererDrawColor r $= mkColor c
  fillRect r $ Just $ mkRect 0 0 (w * s) (h * s)

drawSquare :: Int -> Renderer -> Square -> IO ()
drawSquare _ _ (Square _ _ _ _ Transparent) = return ()
drawSquare size r s@(Square x y _ _ c) = do
  rendererDrawColor r $= mkColor c
  fillRect r $ Just $ mkRect x y size size

drawSprite :: Int -> Renderer -> Sprite -> IO ()
drawSprite size r ((x,y),z,_,t) =
  copy r t Nothing $ Just $ mkRect x y size size

mkRect :: Int -> Int -> Int -> Int -> Rectangle CInt
mkRect x y sx sy = Rectangle (P $ V2 x' y') (V2 sx' sy') where
  x'  = toEnum $ x * sx
  y'  = toEnum $ y * sy
  sx' = toEnum sx
  sy' = toEnum sy

mkColor :: Color -> V4 Word8
mkColor Transparent = V4 1 1 1 0
mkColor (Color r g b) = V4 (toEnum r) (toEnum g) (toEnum b) 255

transToBlack :: Color -> Color
transToBlack Transparent = Color 0 0 0
transToBlack c           = c

evalAction :: Action s -> EngineState s -> EngineState s
evalAction (PaintSquare (x,y) c) s = modSquare (x,y) (setColor c) s
evalAction (CreateSprite xy z name) s =
  case lookup name (texture s) of
    Nothing -> error ("texture: " ++ name ++ " not found")
    Just t  -> s {sprite = (xy,z,name,t) : sprite s}
evalAction (DeleteSprite (x,y)         name) s = 
    s {sprite = filter (not . isSprite (x,y) name) $ sprite s}
    where 
        isSprite :: (Int,Int) -> String -> Sprite -> Bool
        isSprite (x',y') name' ((x,y),_,name,t) = 
          x == x' && y == y' && name == name'
evalAction (MoveSprite   (x,y) (x',y') name) s = 
    let (found,rest) = partition (isSprite (x,y) name) $ sprite s
    in  s {sprite = map (\(_,z,n,t) -> ((x',y'),z,n,t)) found ++ rest}
    where 
        isSprite :: (Int,Int) -> String -> Sprite -> Bool
        isSprite (x',y') name' ((x,y),_,name,t) = 
          x == x' && y == y' && name == name'
evalAction (ChangeStepper s) st = st {gameStepper = s}
evalAction (ChangeHandler h) st = st {gameHandler = h}

modSquare :: (Int,Int) -> (Square -> Square) -> EngineState s -> EngineState s
modSquare (x,y) f st = st {board = modSquare' (board st)} where 
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

