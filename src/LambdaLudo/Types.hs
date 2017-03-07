module LambdaLudo.Types where

import SDL (Texture)
import SDL.Input.Keyboard.Codes (Keycode)

import Control.Monad.Random
import Control.Monad.RWS
import System.Random (StdGen)

data Square = Square
  { x       :: Int
  , y       :: Int
  , name    :: String
  , value   :: Int
  , color   :: Color
  } deriving (Show)
type Board = [Square]

data EngineState s = EngineState
  { frame       :: Int
  , randomState :: StdGen
  , texture     :: [(String,Texture)]
  , sprite      :: [Sprite]
  , board       :: Board
  , boardWidth  :: Int
  , boardHeight :: Int
  , squareSize  :: Int
  , gameStepper :: Step s ()
  , gameHandler :: Handle s ()
  , gameState   :: s
  , gameBg      :: GameBg
  }

data EngineEvent = 
    KeyPress   Keycode
  | MouseClick (Int,Int)
  | MouseHover (Int,Int)
  | Quit
  deriving (Show, Eq)

type Sprite = ((Int,Int),Int,String,Texture)

data Color = Color Int Int Int | Transparent deriving (Show, Eq)

data Action st =
    PaintSquare   (Int,Int) Color
  | CreateSprite  (Int,Int) Int String
  | DeleteSprite  (Int,Int) String
  | MoveSprite    (Int,Int) (Int,Int) String
  | ChangeStepper (Step   st ())
  | ChangeHandler (Handle st ())
  | ChangeBg      GameBg

type Step s a   = RWST (EngineState s) [Action s] s (Rand StdGen) a
type Handle s a = EngineEvent -> Step s a

data GameBg = BgTexture Texture | BgColor Color 

data Config s = Config
  { stepper     :: Step s ()
  , handler     :: Handle s ()
  , initializer :: Step s ()
  , memory      :: s
  , assets      :: [String]
  , columns     :: Int
  , rows        :: Int
  , size        :: Int
  }

 
