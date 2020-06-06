module Save where

import System.Directory
import System.IO()

import GameStruct

-- Сохранение результата в файл
saveResult :: GameState -> IO() 
saveResult state@GS {isSaved = isSaved, run_time = run_time, mineCount = mineCount, name=name} = if not isSaved 
                                  then
                                  	do
                                      createDirectoryIfMissing False "saves"
                                      appendFile resultsFilePath (name ++ " " ++ (show run_time) ++ " "  ++ (show mineCount) ++ "\n")
                                  else
                                    return ()