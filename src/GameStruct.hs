module GameStruct where

import Data.Map
import System.Random
import Data.Set

type Field = Map Cell CellState
type Cell = (Int, Int)
type Mines = Set Cell
type Result = Float
type Name = String
type Mine = Int

data Data = Node Name Result Mine --Класс для хранения результата 

instance Eq Data where
    (Node _ res1 _) == (Node _ res2 _) = res1 == res2
    (Node _ res1 _) /= (Node _ res2 _) = res1 /= res2

instance Ord Data where
    (Node _ res1 _) < (Node _ res2 _) = res1 < res2
    (Node _ res1 _) > (Node _ res2 _) = res1 < res2
    (Node _ res1 _) >= (Node _ res2 _) = res1 < res2
    (Node _ res1 _) <= (Node _ res2 _) = res1 < res2

data CellState = Opened Int -- Открыта; параметр — циферка, которая будет отображаться
                | Mine               -- Мина
                | Flag deriving (Eq) -- Поставлен флажок

data GameProcess = Act -- Действие игры
				| Menu -- Меню
				| Win  -- Победа
 				| Lose -- Поражение
                | Table --Таблица
                | Pause --Пауза
                | Exit deriving (Eq)

data GameState = GS              -- Структура игры 
    { field    :: Field          -- Поле
    , mines    :: Maybe Mines -- Мины если Nothing, то необходимо генерация
    , gameProcess :: GameProcess -- Процесс игры
    , generator :: StdGen       -- Генератор случайных чисел для перезапуска
    , run_time :: Float        -- Время игры
    , mineCount :: Int         --число мин
    , isSaved :: Bool          --Сохранена игра или нет
    , name :: String           --Имя игрока
    , myResults :: Bool        --Показывать собственные результаты
    }


fieldSize :: (Int, Int)
fieldSize@(fieldWidth, fieldHeight) = (15, 15)

cellSize :: Float
cellSize = 30

windowSize:: (Int, Int)
windowSize@(windowWidth, windowHeight) = ((fst fieldSize) * (round cellSize) + 15, 620)

resultsFilePath :: String
resultsFilePath = "saves/results.txt"
