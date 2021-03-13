module Morra where

import Control.Monad.Trans.State
import Control.Monad (void, unless, when)
import System.Random (randomRIO)
import Control.Monad.Trans (liftIO, lift)
import Text.Read (readMaybe)
import Data.Bool (bool)
import Data.Functor ((<&>))

data Role = Odd | Even deriving (Show, Eq)
data Nature = Human | Robot deriving (Show, Eq)

data Player = Player { score :: Int
                     , role :: Role
                     , nature :: Nature
                     , label :: String
                     }

data Game = Game { p1 :: Player
                 , p2 :: Player
                 , lastWinner :: Maybe (Game -> Player)
                 , isGameOver :: Game -> IO Bool
                 }

data FinalScore = Winner Player | Draw

type App = StateT Game IO ()

pickRole :: IO (Role, Role)
pickRole = do
  v <- randomRIO (0 :: Int, 1 :: Int)
  return $ [(Odd, Even), (Even, Odd)] !! v

quitWhenAsked :: Game -> Game
quitWhenAsked g = g { isGameOver = untilQuit }

quitAt :: Int -> Game -> Game
quitAt n g = g { isGameOver = untilScoreIs n }

vsHuman :: Game -> Game
vsHuman g = g { p2 = (p2 g) { nature = Human } }

game :: IO Game
game = do
  (r1, r2) <- pickRole
  return Game { p1 = Player { score = 0
                            , role = r1
                            , nature = Human
                            , label = "P1"
                            }
              , p2 = Player { score = 0
                            , role = r2
                            , nature = Robot
                            , label = "P2"
                            }
              , lastWinner = Nothing
              , isGameOver = untilQuit
              }

finalScore :: Game -> FinalScore
finalScore g = let (Player s1 _ _ _) = p1 g
                   (Player s2 _ _ _) = p2 g
               in case compare s1 s2 of
                    EQ -> Draw
                    LT -> Winner (p2 g)
                    GT -> Winner (p1 g)

play :: Int -> Int -> Game -> Game
play m1 m2 g = let m = m1 + m2
                   (pl, pr, w) = case (odd m, p1 g, p2 g) of
                                  (True, pl@(Player score Odd _ _), pr) ->
                                    (pl { score = score + 1 }, pr, p1)
                                  (True, pl, pr@(Player score Odd _ _)) ->
                                    (pl, pr { score = score + 1 }, p2)
                                  (False, pl@(Player score Even _ _), pr) ->
                                    (pl { score = score + 1}, pr, p1)
                                  (False, pl, pr@(Player score Even _ _)) ->
                                    (pl, pr { score = score + 1}, p2)
               in g { p1 = pl, p2 = pr, lastWinner = Just w }

moveOf :: Game -> (Game -> Player) -> IO Int
moveOf g s = go g (s g)
  where go :: Game -> Player -> IO Int
        go g p@(Player _ _ Human l) = do
          putStr $ l <> ": "
          l <- getLine
          case readMaybe l :: Maybe Int of
            (Just v) | v == 1 || v == 2 -> return v
            _ -> putStrLn "- Wrong input, retry" >> go g p
        go _ (Player _ _ Robot l) = do
          v <- randomRIO (1 :: Int, 2 :: Int)
          putStrLn $ l <> ": " <> show v
          return v

untilQuit :: Game -> IO Bool
untilQuit g = do
  putStr "Wanna go again? (Y/N): "
  l <- getLine
  case l of
    "Y" -> return False
    "N" -> return True
    _ -> putStrLn "- Wrong input, retry" >> untilQuit g

untilScoreIs :: Int -> Game -> IO Bool
untilScoreIs n g = let s1 = score (p1 g)
                       s2 = score (p2 g)
                   in return $ s1 >= n || s2 >= n


showPlayer :: Player -> IO ()
showPlayer (Player _ r n l) =
  putStrLn $ "- " <> l <> " is " <> show n <> ", plays with " <> show r

step :: App
step = do
  g <- get
  m1 <- liftIO $ moveOf g p1
  m2 <- liftIO $ moveOf g p2
  -- g <- state $ \g -> let g' = play m1 m2 g in (g', g')
  modify (play m1 m2)
  g <- get
  liftIO $ showLastWinner g
  -- ???: something better than this, see `ifM`
  isOver <- liftIO (isGameOver g g)
  if isOver
    then liftIO (showFinalScore g)
    else step
  -- liftIO $ when isOver (showFinalScore g)
  -- unless isOver step

showLastWinner :: Game -> IO ()
showLastWinner g = do
  case lastWinner g of
    (Just w) ->
      putStrLn $ "- Wins: " <> label (w g)
    Nothing ->
      return ()

showFinalScore :: Game -> IO ()
showFinalScore g = do
  showScore g
  case finalScore g of
    (Winner (Player s r n l)) ->
      putStrLn $ "- Winner is " <> l
    Draw ->
      putStrLn "- It's a draw!"

showScore :: Game -> IO ()
showScore g = do
  putStrLn $ "- " <> label (p1 g) <> " score is " <> show (score (p1 g))
  putStrLn $ "- " <> label (p2 g) <> " score is " <> show (score (p2 g))

main :: IO ()
main = do
  -- g <- game <&> quitAt 3 <&> vsHuman
  g <- game <&> quitAt 3
  showPlayer $ p1 g
  showPlayer $ p2 g
  runStateT step g
  return ()

-- TODO: remember history of moves, pick next move based on opponent history
-- TODO: ask for player name?
