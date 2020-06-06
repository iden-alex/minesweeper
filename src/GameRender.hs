module GameRender where

import Data.Map
import GameStruct
import GameResults
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


renderer :: GameState -> IO Picture
-- Отрисовка меню
renderer GS 
    {gameProcess = Menu
    } = return (applyViewPortToPicture viewPort (pictures 
        [menu
        , newGame
        , easyMod
        , mediumMod
        , hardMod
        , scoreTable
        , exit
        ])) where
            menu = translate 148 410  (scale 0.3 0.3 (color (greyN 0.85) (text "MENU"))) 
            newGame = translate 10 320  (scale 0.3 0.3 (color (greyN 0.85) (text "[E] New game:")))
            easyMod = translate 90 275 (scale 0.2 0.2 (color (greyN 0.85) (text "- [E] easy ")))
            mediumMod = translate 90 235 (scale 0.2 0.2 (color (greyN 0.85) (text "- [M] medium")))
            hardMod = translate 90 195 (scale 0.2 0.2 (color (greyN 0.85) (text "- [H] hard")))
            scoreTable = translate 10 140 (scale 0.3 0.3 (color (greyN 0.85) (text "[T] Score table ")))
            exit = translate 10 70 (scale 0.3 0.3 (color (greyN 0.85) (text "[Esc] Exit ")))

-- Отрисовка таблицы результатов
renderer GS 
    {gameProcess = Table
    ,mineCount = mineCount
    ,name = name
    ,myResults = myResults
    } = do
        line <- getText resultsFilePath
        let 
            score1 = translate 10 300 (scale 0.2 0.2 (color (greyN 0.85) (text res1)))
            score2 = translate 10 250 (scale 0.2 0.2 (color (greyN 0.85) (text res2)))
            score3 = translate 10 200 (scale 0.2 0.2 (color (greyN 0.85) (text res3)))
            score4 = translate 10 150 (scale 0.2 0.2 (color (greyN 0.85) (text res4)))
            score5 = translate 10 100 (scale 0.2 0.2 (color (greyN 0.85) (text res5)))
            (tmp:res2:res3:res4:res5:oth) = (getBestResults line 5 mineCount name myResults) ++ (repeat " ")
            res1 = if tmp == " " then (if myResults then "No results." else "No results. Be the first!") else tmp
        return (applyViewPortToPicture viewPort (pictures 
            [title
            , subtitle
            , score1
            , score2
            , score3
            , score4
            , score5
            , keys
            , switch
            ])) where
            title = translate 80 410  (scale 0.3 0.3 (color (greyN 0.85) (text "Top 5 results"))) 
            subtitle = translate 10 360  (scale 0.2 0.2 (color (greyN 0.85) (text mode)))
            keys = translate 10 0  (scale 0.2 0.2 (color (greyN 0.85) (text "| [F1] menu, [E,M,H] mode |")))
            switch 
                | myResults = translate 10 (-50)  (scale 0.215 0.2 (color (greyN 0.85) (text "| [n] switch to all results |")))
                | otherwise = translate 10 (-50)  (scale 0.205 0.2 (color (greyN 0.85) (text "| [n] switch to my results |")))
            mode = case mineCount of
                16 -> "Easy mode"
                25 -> "Medium mode"
                36 -> "Hard mode"

renderer GS 
    { gameProcess = Exit
    } = return (applyViewPortToPicture viewPort (pictures [exiting])) where
        exiting = translate 30 200  (scale 0.3 0.3 (color (greyN 0.85) (text (""))))

-- Отрисовка основной части игры
renderer GS 
    { field = field
    , gameProcess = gameProcess
    , run_time = run_time
    , mineCount = mineCount
    } = return (applyViewPortToPicture viewPort (pictures (
        [drawUpText gameProcess
        ,hint] 
        ++ drawfield))) where -- Отрисовка клеток и надписей
    grid = [uncurry translate (cellToScreen (x, y)) (color white (rectangleWire cellSize cellSize)) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) (drawCell x y) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    hint = translate 25 (-60) (scale 0.2 0.2 (color (greyN 0.85) (text "| [F1] menu   [P] pause |")))
    pausing = translate 150 200  (scale 0.3 0.3 (color (greyN 0.85) (text "PAUSE")))
    drawfield = case gameProcess of
        Pause -> [pausing]
        otherwise -> cells ++ grid
    drawCell x y = case Data.Map.lookup (x, y) field of
        Prelude.Nothing         -> color (greyN 0.1) (rectangleSolid cellSize cellSize)-- Клетка пустая
        Just Mine       -> pictures [ color red (rectangleSolid cellSize cellSize)
                                    , label "X"
                                    ]
        Just (Opened n) -> pictures [ color (greyN 0.85) (rectangleSolid cellSize cellSize)
                                    , label (show n)
                                    ]
        Just Flag       -> pictures [ color orange (rectangleSolid cellSize cellSize)
                                    , label "?"
                                    ]
    label = translate (-5) (-5) . scale 0.15 0.15 . color black . text                                    
    drawUpText s = case s of -- Отрисовка текста сверху в зависимости от состояния
        Win  -> translate 10 (fromIntegral fieldHeight * cellSize ) (scale 0.3 0.3 ( color green (text ("Your score: " ++ (show (round run_time)) ++ "s"))))
        Lose -> translate 110 (fromIntegral fieldHeight * cellSize ) (scale 0.35 0.35 ( color red (text "You lose")))
        otherwise -> drawScore (countFlags field) mineCount run_time

-- Отрисовка количества флагов, мин, времени
drawScore :: Int -> Int -> Float -> Picture 
drawScore flags mineCount tm = translate 0 h (scale 0.3 0.3 (color (greyN 0.85) (text scoreString))) where
    scoreString = ("Mines: " ++ (show flags) ++ "/"  ++ (show mineCount) ++ "   " ++ (show (round tm)))
    h = fromIntegral fieldHeight * cellSize

-- Кол-во флагов на поле
countFlags ::  Field -> Int
countFlags f = Data.Map.size (Data.Map.filter (== Flag) f)

-- Координаты клетки
cellToScreen :: (Int, Int) -> (Float, Float)
cellToScreen = both ((* cellSize) . fromIntegral)

-- Вспомогательная функция
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

-- Функция для преобразования координат 
viewPort :: ViewPort 
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) (cellToScreen fieldSize)) 0 1
