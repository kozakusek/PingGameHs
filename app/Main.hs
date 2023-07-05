{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (MVar, QSem, forkIO, isEmptyMVar, newEmptyMVar, newMVar, newQSem, putMVar, readMVar, signalQSem, swapMVar, takeMVar, threadDelay, waitQSem)
import Control.Concurrent.Async (mapConcurrently)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Serialize (Serialize, decode, encode)
import Data.Time.Clock (UTCTime (utctDayTime), getCurrentTime)
import GHC.Generics (Generic)
import Net (connectToServer, waitForConnection)
import Network.Socket (Socket, close)
import Network.Socket.ByteString (recv, sendAll)
import Parameters
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (BufferMode (LineBuffering, NoBuffering), hReady, hSetBuffering, hSetEcho, stdin)
import System.Random (StdGen, mkStdGen, randomR)
import System.Timeout (timeout)

data PingGameT a = Game
  { ballPos :: (a, a),
    ballVel :: (a, a),
    playersPos :: (a, a),
    playersScores :: (Int, Int),
    pausedFrames :: Int
  }
  deriving (Generic)

type PingGame = PingGameT Float

type PingGameInt = PingGameT Int

data Move = Up | Down | Still deriving (Show, Eq)

instance Serialize PingGame

castPingGame :: PingGame -> PingGameInt
castPingGame g = Game (cast $ ballPos g) (cast $ ballVel g) (cast $ playersPos g) (playersScores g) (pausedFrames g)
  where
    cast :: (Float, Float) -> (Int, Int)
    cast (x, y) = (round x, round y)

genRandVel :: StdGen -> ((Float, Float), StdGen)
genRandVel rng = ((x, y), rng')
  where
    (angle, rng') = randomR (0, 2 * pi) rng
    x = cos angle * ballVelocity
    y = sin angle * ballVelocity

initState :: StdGen -> (PingGame, StdGen)
initState rng =
  let (v, rng') = genRandVel rng
   in ( Game
          { ballPos = middle,
            ballVel = v,
            playersPos = (heightF / 2, heightF / 2),
            playersScores = (0, 0),
            pausedFrames = 0
          },
        rng'
      )

move2vel :: Move -> Float
move2vel Up = -paddleVelocity
move2vel Down = paddleVelocity
move2vel Still = 0

isGameFinished :: PingGame -> Bool
isGameFinished g = fst (playersScores g) == maxPoints || snd (playersScores g) == maxPoints

m2bs :: Move -> ByteString
m2bs Up = "U"
m2bs Down = "D"
m2bs Still = "S"

bs2m :: ByteString -> Move
bs2m "U" = Up
bs2m "D" = Down
bs2m _ = Still

char2m :: Char -> Move
char2m 'U' = Up
char2m 'D' = Down
char2m _ = Still

displayGameState :: PingGame -> IO ()
displayGameState game = do
  resetDisplay
  let score = show (fst $ playersScores g) ++ show (snd $ playersScores g)
  let pad = replicate (width - length score) ' '
  putStrLn $ show (fst $ playersScores g) ++ pad ++ show (snd $ playersScores g)
  putStrLn $ replicate width wall
  mapM_ (putStrLn . displayLine) [0 .. height - 1]
  putStrLn $ replicate width wall
  where
    g = castPingGame game
    displayLine y = [displayChar (x, y) | x <- [0 .. width - 1]]
    displayChar (x, y)
      | (x, y) == ballPos g = ball
      | x == 0 && fst (playersPos g) - paddleRadius <= y && y < fst (playersPos g) + paddleRadius = leftPaddle
      | x == width - 1 && snd (playersPos g) - paddleRadius <= y && y < snd (playersPos g) + paddleRadius = rightPaddle
      | otherwise = space

updateGameState :: PingGame -> Move -> Move -> StdGen -> IO (PingGame, StdGen)
updateGameState g p1 p2 rng =
  let pausedFrames' = pausedFrames g + 1
   in if pausedFrames' < pauseLen
        then return (g {pausedFrames = pausedFrames'}, rng)
        else gaming g p1 p2 rng
  where
    gaming g p1 p2 rng = do
      let playersPos' = (cropY $ fst (playersPos g) + move2vel p1, cropY $ snd (playersPos g) + move2vel p2)
      let ballPos' = ballPos g `add` ballVel g
      case scoresPoints ballPos' playersPos' of
        Just (p1', p2') -> do
          let playersScores' = (fst (playersScores g) + p1', snd (playersScores g) + p2')
          let (g', rng') = initState rng
          return (g' {playersPos = playersPos', playersScores = playersScores'}, rng')
        Nothing -> do
          let ballVel' = bimap (xVelChange ballPos' *) (yVelChange ballPos' *) (ballVel g)
          return (Game (fixBallPos ballPos') ballVel' playersPos' (playersScores g) 0, rng)

    scoresPoints (ballX, ballY) (p1Y, p2Y)
      | ballX <= 0 && not (p1Y - paddleRadiusF <= ballY && ballY < p1Y + paddleRadiusF) = Just (0, 1)
      | ballX >= widthF - 1 && not (p2Y - paddleRadiusF <= ballY && ballY < p2Y + paddleRadiusF) = Just (1, 0)
      | otherwise = Nothing

    fixBallPos (ballX, ballY) = (max 1 $ min (widthF - 2) ballX, max 0 $ min (heightF - 1) ballY)

    yVelChange (_, ballY) = if ballY <= -1 || ballY >= heightF then -1 else 1
    xVelChange (ballX, _) = if ballX <= 0 || ballX >= widthF - 1 then -1 else 1

    cropY y = max 2 $ min (heightF - 2) y
    add (a, b) (c, d) = (a + c, b + d)

-- ** SERVER ** --

server :: IO ()
server = do
  conn <- waitForConnection

  seed <- getCurrentTime <&> (floor . utctDayTime)
  let (g, rng) = initState (mkStdGen seed)

  g' <- serverGameLoop conn g rng

  close conn
  goodbye g' (>)

serverGameLoop :: Socket -> PingGame -> StdGen -> IO PingGame
serverGameLoop sock g rng = do
  sem <- newQSem 0

  (stop, move) <- startCapturing
  g' <- innerLoop sock g sem move rng
  stopCapturing stop

  return g'
  where
    innerLoop sock g sem move rng = do
      [u, c, _] <- mapConcurrently (\f -> f sem) [readingUser move, readingClient sock, waitFrame]
      (g', rng') <- updateGameState g u c rng
      sendAll sock $ encode g'
      displayGameState g'
      if isGameFinished g' then return g' else innerLoop sock g' sem move rng'

readingClient :: Socket -> QSem -> IO Move
readingClient sock sem = do
  waitQSem sem
  msg <- recv sock 1024
  return $ char2m $ BC.last msg

readingUser :: MVar Move -> QSem -> IO Move
readingUser move sem = do
  waitQSem sem
  readMVar move

waitFrame :: QSem -> IO Move
waitFrame sem = do
  threadDelay oneSecond
  signalQSem sem
  signalQSem sem
  return Still

-- ** CLIENT ** --

client :: IO ()
client = do
  conn <- connectToServer

  let (g, _) = initState (mkStdGen 0)

  (stop, move) <- startCapturing
  g' <- clientGameLoop conn g move
  stopCapturing stop

  close conn
  goodbye g' (<)

clientGameLoop :: Socket -> PingGame -> MVar Move -> IO PingGame
clientGameLoop sock g move = do
  gameState <- newEmptyMVar

  forkIO $ clientReadUser sock gameState move
  forkIO $ clientDisplayer sock g gameState

  takeMVar gameState

clientReadUser :: Socket -> MVar a -> MVar Move -> IO ()
clientReadUser sock stop move = do
  threadDelay 8333
  key <- readMVar move
  sendAll sock $ m2bs key
  inProgress <- isEmptyMVar stop
  if not inProgress then return () else clientReadUser sock stop move

clientDisplayer :: Socket -> PingGame -> MVar PingGame -> IO ()
clientDisplayer sock g gameState =
  if isGameFinished g
    then putMVar gameState g
    else do
      msg <- recv sock 1024

      let g' :: PingGame = fromRight undefined $ decode msg
      displayGameState g'

      clientDisplayer sock g' gameState

-- ** OTHER ** --

goodbye :: PingGame -> (Int -> Int -> Bool) -> IO ()
goodbye g cmp = do
  resetDisplay
  putStrLn $
    if uncurry cmp (playersScores g)
      then "Congratulations!"
      else "Better luck next time."
  putStrLn $ show (fst $ playersScores g) ++ " : " ++ show (snd $ playersScores g)
  putStrLn "Press Enter to continue..."
  getLine
  return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char : chars)

startCapturing :: IO (MVar (), MVar Move)
startCapturing = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  stop <- newEmptyMVar
  move <- newMVar Still
  forkIO $ captureInput stop move
  return (stop, move)
  where
    captureInput stop move = do
      key <- timeout 500000 getKey
      case key of
        Just "w" -> swapMVar move Up
        Just "W" -> swapMVar move Up
        Just "s" -> swapMVar move Down
        Just "S" -> swapMVar move Down
        _ -> swapMVar move Still
      inProgress <- isEmptyMVar stop
      if not inProgress then return () else captureInput stop move

stopCapturing :: MVar () -> IO ()
stopCapturing stop = do
  hSetEcho stdin True
  hSetBuffering stdin LineBuffering
  putMVar stop ()

resetDisplay :: IO ()
resetDisplay = setCursorPosition 0 0 >> clearScreen

showMenu :: IO ()
showMenu = do
  resetDisplay
  putStrLn logo
  putStrLn menu

main :: IO ()
main = do
  showMenu
  choice <- getLine
  case choice of
    "1" -> server >> main
    "2" -> client >> main
    "0" -> putStrLn "Exiting..."
    _ -> main
