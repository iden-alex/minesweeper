module GameResults where

import Graphics.Gloss.Interface.IO.Game
import System.Directory
import GameStruct
import Data.List

-- Получение строки с текста
getText ::  String -> IO String 
getText path = do
                    fileExists <- doesFileExist path
                    if fileExists
                    then do
                        res <- readFile path
                        if null res
                        then
                            return ""
                        else
                            return res
                    else
                        return ""

-- Перевод всех результатов
getResults :: String -> [Data] 
getResults text = map toFormat (map Prelude.words (Prelude.lines text))

-- Перевод из строки в Data
toFormat :: [String] -> Data 
toFormat (name:x:y:z) = (Node name time mines) where
		time = read x::Float
		mines = read y::Int

-- Получение лучших результатов
getBestResults :: String -> Int -> Int -> Name -> Bool -> [String] 
getBestResults text num mines name my_res 
    | my_res = map toStr (sort (Prelude.filter (\ (Node n _ m) -> (m == mines) && (n == name)) (getResults text)))
    | otherwise = map toStr (sort (Prelude.filter (\ (Node _ _ m) -> m == mines) (getResults text)))

toStr :: Data -> String
toStr (Node name time _) = (show (round time)) ++ "s  - " ++ name