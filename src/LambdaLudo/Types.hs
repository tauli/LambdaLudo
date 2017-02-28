module LambdaLudo.Types where

import SDL (Texture)
import SDL.Input.Keyboard.Codes (Keycode)

import Control.Monad.Random
import Control.Monad.RWS

data Square = Square
  { x       :: Int
  , y       :: Int
  , name    :: String
  , value   :: Integer
  , color   :: Color
  } deriving (Show)
type Board = [Square]

data EngineState = EngineState
  { board   :: Board
  , frame   :: Int
  , texture :: [(String,Texture)]
  , sprite  :: [Sprite]
  }

type Sprite = ((Int,Int),Int,String,Texture)

data Color = Color Int Int Int | Transparent deriving (Show, Eq)

data Action =
    PaintSquare  (Int,Int) Color
  | CreateSprite (Int,Int) Int String
  | DeleteSprite (Int,Int) String
  | MoveSprite   (Int,Int) (Int,Int) String
    deriving (Eq)

type Step s a   = RWST EngineState [Action] s (Rand StdGen) a
type Handle s a = Keycode -> Step s a

data Config s = Config
  { stepper     :: Step s ()
  , handler     :: Handle s ()
  , initializer :: Step s ()
  , initState   :: s
  , bgColor     :: Color
  , assets      :: [String]
  , columns     :: Int
  , rows        :: Int
  , size        :: Int
  }


