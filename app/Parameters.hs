module Parameters where

maxPoints :: Int
maxPoints = 3

paddleRadius, width, height :: Int
paddleRadius = 2
width = 90
height = 26

paddleRadiusF, widthF, heightF :: Float
paddleRadiusF = fromIntegral paddleRadius
widthF = fromIntegral width
heightF = fromIntegral height

middle :: (Float, Float)
middle = (widthF / 2, heightF / 2)

wall, ball, leftPaddle, rightPaddle, space :: Char
wall = 'X'
ball = '*'
leftPaddle = ']'
rightPaddle = '['
space = ' '

logo :: String
logo =
  "  ____              _   _     ____   \n"
    ++ "U|  _\"\\ u  ___     | \\ |\"| U /\"___|u \n"
    ++ "\\| |_) |/ |_\"_|   <|  \\| |>\\| |  _ / \n"
    ++ " |  __/    | |    U| |\\  |u | |_| |  \n"
    ++ " |_|     U/| |\\u   |_| \\_|   \\____|  \n"
    ++ " ||>>_.-,_|___|_,-.||   \\,-._)(|_   \n"
    ++ "(__)__)\\_)-' '-(_/ (_\")  (_/(__)__)  \n"

menu :: String
menu =
  "\t1. Player 1 (server)\n"
    ++ "\t2. Player 2 (client)\n"
    ++ "\n"
    ++ "\t0. Exit\n"

fps :: Int
fps = 10

frameTime :: Int
frameTime = 100000 `div` fps

pauseLen :: Int
pauseLen = fps

ballVelocity :: Float
ballVelocity = 2

paddleVelocity :: Float
paddleVelocity = 1.3
