module GameInit where

import System.Random.Shuffle (shuffle')
import Data.Map
import Data.Set
import System.Random
import GameStruct

createField :: Field
createField = Data.Map.empty

-- Перемешивание списка для генерации мин
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g l = shuffle' l (fieldWidth * fieldHeight - 1) g

initState :: StdGen -> String -> GameState
initState gen name = GS createField Nothing Menu gen 0 15 False name False

-- Создание множества мин 
createMines :: RandomGen g => g -> Cell -> Int -> Mines
createMines g fst mineCount = Data.Set.fromList $ Prelude.take mineCount (shuffle g (
    [(i, j) | i <- [0 .. fieldWidth - 1]
            , j <- [0 .. fieldHeight - 1]
            , (i, j) /= fst]))

-- Получение имени игрока
getName :: IO String
getName = do
    putStrLn "Enter your name: "
    name <- getLine
    if ' ' `elem` name
        then do
            putStrLn "Spaces are not allowed!"
            getName
        else if (length name) > 20
            then do 
                putStrLn "The name must be less than 20 characters."
                getName
            else return name