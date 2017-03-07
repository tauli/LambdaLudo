{-# LANGUAGE FlexibleContexts #-}

module LambdaLudo.EDSL where

import LambdaLudo.Types

import SDL (Texture)

import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State as S


findSquare :: (Int,Int) -> Board -> Maybe Square
findSquare (x,y) b = case filter matchSquare b of
  (s:_) -> Just s
  _     -> Nothing
  where
    matchSquare :: Square -> Bool
    matchSquare (Square x' y' _ _ _) = x == x' && y == y'

defaultHandler :: Handle s ()
defaultHandler _ = nop

nop :: Step s ()
nop = return ()

readColor :: RWS.MonadReader (EngineState s) m => (Int,Int) -> m Color
readColor c = do
  b <- RWS.reader board
  case findSquare c b of
    Just (Square _ _ _ _ (Color r g b)) -> return $ Color r g b
    _                                   -> return Transparent

readFrameNr :: RWS.MonadReader (EngineState s) m => m Int
readFrameNr = RWS.reader frame

findSpriteByXY :: RWS.MonadReader (EngineState s) m => (Int,Int) -> m [String]
findSpriteByXY (x,y) = do
  s <- RWS.reader sprite
  return $ map (\(_,_,name,_) -> name) $ filter (isSprite (x,y)) s where
    isSprite :: (Int,Int) -> Sprite -> Bool
    isSprite (x',y') ((x,y),_,name,t) = x == x' && y == y'

findSpriteByName :: RWS.MonadReader (EngineState s) m => String -> m [(Int,Int)]
findSpriteByName name = do
  s <- RWS.reader sprite
  return $ map (\(xy,_,_,_) -> xy) $ filter (isSprite name) s where
    isSprite :: String -> Sprite -> Bool
    isSprite name' ((x,y),_,name,t) = name == name'

paintSquare :: RWS.MonadWriter [Action st] m => (Int,Int) -> Color -> m ()
paintSquare xy c = RWS.tell [PaintSquare xy c]

createSprite :: RWS.MonadWriter [Action st] m 
                => (Int,Int) -> Int -> String -> m ()
createSprite xy z sName = RWS.tell [CreateSprite xy z   sName]

deleteSprite :: RWS.MonadWriter [Action st] m => (Int,Int) -> String -> m ()
deleteSprite xy sName = RWS.tell [DeleteSprite xy sName]

moveSprite :: RWS.MonadWriter [Action st] m 
              => (Int,Int) -> (Int,Int) -> String -> m ()
moveSprite xy xy' sName = RWS.tell [MoveSprite xy xy' sName]

changeStepper :: RWS.MonadWriter [Action st] m => Step st () -> m ()
changeStepper s = RWS.tell [ChangeStepper s]

changeHandler :: RWS.MonadWriter [Action st] m => Handle st () -> m ()
changeHandler h = RWS.tell [ChangeHandler h]

setBackgroundImage :: RWS.MonadWriter [Action st] m => Texture -> m ()
setBackgroundImage t = RWS.tell [ChangeBg $ BgTexture t]

setBackgroundColor :: RWS.MonadWriter [Action st] m => Color -> m ()
setBackgroundColor c = RWS.tell [ChangeBg $ BgColor c]
