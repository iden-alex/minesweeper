{-# LANGUAGE BlockArguments #-}
module GameAction where

import GameStruct
import GameInit
import GameRender
import GameResults
import Save
import Data.List
import Data.Map
import Data.Set
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import Graphics.Gloss.Interface.IO.Game
import System.Directory
import System.Exit

startGame :: StdGen -> IO ()
startGame gen = do
    name <- getName
    playIO (InWindow "SAPER" windowSize (10, 10)) (greyN 0.1) 1 (initState gen name) renderer handler updater 

-- Обработка обычных клавиш 
handler :: Event -> GameState -> IO GameState
handler (EventKey (Char key) Down _ _) gs@GS 
    { gameProcess = proc
    , myResults = fl
    , mineCount = mines
    } = case proc of
        Menu -> case key of
            'e'->return gs {gameProcess = Act, mineCount = 16}
            'm'->return gs {gameProcess = Act, mineCount = 25}
            'h'->return gs {gameProcess = Act, mineCount = 36}
            't'->return gs {gameProcess = Table, mineCount = 16}
            otherwise -> return gs
        Table -> case key of
            'e' -> return gs {gameProcess = Table, mineCount = 16}
            'm' -> return gs {gameProcess = Table, mineCount = 25}
            'h' -> return gs {gameProcess = Table, mineCount = 36}
            'n' -> return gs {gameProcess = Table, mineCount = mines, myResults = (not fl)}
            otherwise -> return gs
        Act -> case key of
            'p'-> return gs {gameProcess = Pause}
            otherwise -> return gs
        Pause -> case key of
            'p'-> return gs {gameProcess = Act}
            otherwise -> return gs
        otherwise -> return gs

-- Генерация мин при первом нажатии мышкой на поле
handler (EventKey (MouseButton LeftButton) Down x mouse) gs@GS 
    { mines = Nothing
    , generator = gen
    , gameProcess = Act
    , mineCount = mineCount
    } = do
        let cell = screenToCell mouse
        res <- handler (EventKey (MouseButton LeftButton) Down x mouse) gs { mines = Just (createMines gen cell mineCount), gameProcess = Act} 
        return res

 -- Открытие ячейки поля
handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { field = field
    , mines = Just mines
    , gameProcess = Act
    , mineCount = mineCount
    } = return gs
    { field = newField
    , gameProcess = exploded
    } where
    newField = click cell field
    exploded | Data.Map.lookup cell newField == Just Mine                      = Lose
             | countOpenCells newField >= fieldHeight * fieldWidth - mineCount = Win
             | otherwise                                                       = Act
    countOpenCells f = Data.Map.size (Data.Map.filter (/= Flag) f)
    cell@(cx, cy) = screenToCell mouse
    click :: Cell -> Field -> Field
    click c@(cx, cy) f
        | Data.Map.member c f     = f --повторно клетку не обрабатываем
        | Data.Set.member c mines =  Data.Map.union f (Data.Map.fromSet (\k -> Mine) mines) --попались на мину
        | otherwise = let nf = put (Opened neighbours) in
            if neighbours == 0
                then Prelude.foldr click nf neighbourCells --Обойдём соседей
                else nf
        where
            put state = Data.Map.insert c state f
            neighbourCells = [ (i, j) | i <- [cx - 1 .. cx + 1], j <- [cy - 1 .. cy + 1]
                             , 0 <= i && i < fieldWidth
                             , 0 <= j && j < fieldHeight
                             ]
            neighbours = length (Prelude.filter (`Data.Set.member` mines) neighbourCells)

 -- Поставновка флагов правой клавишей мышки
handler (EventKey (MouseButton RightButton) Down _ mouse) gs@GS
    { field = field
    , gameProcess = Act
    , mineCount = mineCount
    } = case Data.Map.lookup coord field of
        Nothing -> if (countFlags field) < mineCount -- проверка на кол-во флагов
            then return gs { field = Data.Map.insert coord Flag field }
            else return gs
        Just Flag -> return gs { field = Data.Map.delete coord field }
        _ -> return gs
        where coord = screenToCell mouse

-- Обработка специальных клавишей
handler (EventKey (SpecialKey key) Down _ _) gs@GS
    { generator = gen 
    } = case key of
        KeyF1 -> return gs { field = createField
                            , mines = Nothing
                            , gameProcess = Menu
                            , generator = snd (next gen)
                            , run_time = 0
                            , isSaved = False
                            }
        KeyEsc -> return gs { gameProcess = Exit}
        otherwise -> return gs

handler _ gs = return gs

-- Обновление состояния игры в каждую единицу времени
updater :: Float -> GameState -> IO GameState
updater s gs@GS 
    { mines = mines 
    , gameProcess = proc
    , run_time = tmp
    , isSaved = isSaved
    }   | proc == Act && (mines /= Nothing) = return gs { run_time = tmp + s }
        | proc == Win && isSaved == False =  do
                                                saveResult gs        
                                                return gs{isSaved = True}
        | proc == Exit = do
                            exitSuccess
                            return gs
        | otherwise = return gs

screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort
