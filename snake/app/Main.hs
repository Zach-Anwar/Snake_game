module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

window:: Display
window = InWindow "background" (1000, 600) (0, 0)

type SnakeTile = (Int, Int) --represents the coordinates of a specific square used to draw par of the snake's coordinates and apples coordinates
type Snake = [SnakeTile] --a list of SnakeTiles is used to make up the snake
data Direction = UP | DOWN | RIGHT | LEFT deriving (Eq, Ord) --used to store the direction the snake is travelling derives
--(Eq, Ord) so we can compare directions with each easily


data Grid = Grid --stores data about the current game
    { applePlace :: SnakeTile --the location of the apple
    , snakePlace :: Snake --the location of the snake
    , facing :: Direction --the direction the snake is currently facing
    , store :: Direction --stores the most recently input direction by the user using the arrow keys
    , expand :: Bool --true if the snake should increase in size otherwise it is false
    , nextRandom :: StdGen --stores the StdGen generated when creating the last random appleplace so it can be used
    --to generate another set of random coordinates again
    }

startGrid :: Grid --start of the game have the snake in the centre facing down. Creates stdgens so apple can be randomly
--placed in different locations
startGrid = Grid (fst (randomGenerator (mkStdGen 70) snake)) snake DOWN DOWN False (mkStdGen 100)
    where
        snake = [(0,x) | x <- [0, 4 .. 160]] --creates a list of coordinates so that there is a snake tile fore every 4x the snake spans

randomGenerator :: StdGen -> [SnakeTile] -> ((Int, Int ), StdGen) --generates random set coordinates for a new apple position
randomGenerator stdGen snake
                    |(randomX * 40, randomY * 40) `elem` snake = randomGenerator stdGen2 snake --if new coordiantes are within the snake generates new place for the apple
                    |otherwise  = ((randomX, randomY), stdGen2) --otherwise returns new coordinates
    where
        (randomX, stdGen1) = randomR (-12, 12) stdGen --generates coordinates using the prevuous stdgen
        (randomY, stdGen2) = randomR (-7, 7) stdGen1

drawSnakeTile :: SnakeTile -> Picture --draws a given snake tile onto the screen
drawSnakeTile (x, y) = color green $ translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 40 40 --color of the snake is green
--tile needs to be positioned at the coordinate fiven to it and every tile is a solid square of 40 lengh

drawAppleTile :: SnakeTile -> Picture --draws an apple to the screen
drawAppleTile (x, y) = color red $ translate (fromIntegral (x * 40)) (fromIntegral (y * 40)) $ circleSolid 20
--exactly the same as snake tile

gameOver :: Snake -> Bool --determines if the game is over
gameOver ((x, y):xs) 
                |x < (-12 * 40) || y < (-7 * 40) || x > (12 * 40) || y > (7 * 40) = True --if the nsale has gone out of bounds end the current game
                |(x, y) `elem` xs = True --if the head of the snake has got to the same coordinate as some part of tail it has hit itself so end the game
                |otherwise = False --otherwise the game continues
gameOver [] = True

drawGrid :: Grid -> Picture --draws a given game state 
drawGrid currentGame = Pictures $ drawAppleTile apple : map drawSnakeTile snake --draws the apple and draws every individual
--snaketile in the Snake datatype
    where
        snake = snakePlace currentGame --gets the snake and apple of the current game
        apple = applePlace currentGame

handleInput :: Event -> Grid -> Grid --takes in the input from the user
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (Grid apple snake currentDirection store expand stdgen)  = 
    if currentDirection == LEFT then Grid apple snake currentDirection store expand stdgen else Grid apple snake currentDirection RIGHT expand stdgen

handleInput (EventKey (SpecialKey KeyUp) Down _ _) (Grid apple snake currentDirection store expand stdgen) = 
    if currentDirection == DOWN then Grid apple snake currentDirection store expand stdgen else Grid apple snake currentDirection UP expand stdgen

handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (Grid apple y currentDirection store expand stdgen)  = 
    if currentDirection == RIGHT then Grid apple y currentDirection store expand stdgen else Grid apple y currentDirection LEFT expand stdgen

handleInput (EventKey (SpecialKey KeyDown) Down _ _) (Grid apple snake currentDirection store expand stdgen) = 
    if currentDirection == UP then Grid apple snake currentDirection store expand stdgen else Grid apple snake currentDirection DOWN expand stdgen

--for every possible arrow key input it takes the input and puts it into the store variable in the Grid data structure
--as long as that direction is no the opposite direction to the snakes current direction
handleInput _ x = x

update :: Float -> Grid -> Grid --updates the snake and apple location for the next grid
update seconds (Grid (m,n) ((x,y):xs) direction store expand z) 
    |gameOver ((x,y):xs) = startGrid
    |direction == RIGHT = Grid apple (move ((x,y):xs) (x + 4, y)) newDirection store shouldExpand stdGen
    |direction == LEFT = Grid apple (move ((x,y):xs) (x - 4, y)) newDirection store shouldExpand stdGen
    |direction == UP = Grid apple (move ((x,y):xs) (x, y + 4)) newDirection store shouldExpand stdGen
    |otherwise = Grid apple (move ((x,y):xs) (x, y - 4)) newDirection store shouldExpand stdGen
        where
            shouldExpand --determines if the snake should increse in size (the head of the tail has just eaten an apple)
                |(m * 40, n * 40) == (x,y) = True
                |x `mod` 40 == 0 && y `mod` 40 == 0 = False --after the snake has moved the equivalent of one tile the snake should stop expanding
                |otherwise  = expand --otherwise it should not change weather the snake is expaniding or not
            apple
                |(m * 40, n * 40) == (x, y) = fst (randomGenerator z ((x,y):xs)) --id the snake has just eaten an apple find a new random place for it otherwise leave it unchanged
                |otherwise = (m, n)
            stdGen
                |(m * 40, n * 40) == (x, y)= snd (randomGenerator z ((x,y):xs)) --generates a new stdgen so the next apple placement will be random
                |otherwise = z
            move
                |shouldExpand = addToSnake
                |otherwise  = moveSnake
            newDirection
                |x `mod` 40 == 0 && y `mod` 40 == 4 && direction == DOWN = store
                |x `mod` 40 == 4 && y `mod` 40 == 0 && direction == LEFT = store
                |x `mod` 40 == 0 && y `mod` 40 == 36 && direction == UP = store
                |x `mod` 40 == 36 && y `mod` 40 == 0 && direction == RIGHT = store
                |otherwise = direction
update _ _ = startGrid

moveSnake :: Snake -> SnakeTile -> Snake --moves the snake to its next postion for the next Grid in the game
moveSnake (z:zs) previous = previous: moveSnake zs z 
--previous at first stores the new location of the head and places it at the head of the new list of SnakeTiles. Then it recursively
--puts every element of the input snake data type
moveSnake [] previous = []

addToSnake :: Snake -> SnakeTile -> Snake
addToSnake xs next = next:xs --adds a new snake tile to the head of the snake in the direction the snake is moving causing it to increase in size

main :: IO ()
main = play window black 60 startGrid drawGrid handleInput update --play function manages Inputs and Outputs of the game further specified in the functions above
 