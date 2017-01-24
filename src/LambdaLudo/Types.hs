module LambdaLudo.Types where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

data Square = Square
  { x       :: Int
  , y       :: Int
  , name    :: String
  , value   :: Integer
  , color   :: Color
  } deriving (Show)
type Board = [Square]

data GameState = GameState
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

type Step a   = ReaderT GameState (Writer [Action]) a
type Handle a = Keycode -> Step a

data Config = Config
  { stepper     :: Step ()
  , handler     :: Handle ()
  , initializer :: Step ()
  , bgColor     :: Color
  , assets      :: [String]
  , columns     :: Int
  , rows        :: Int
  , size        :: Int
  }


