module Lib
    ( someFunc
    ) where

import Test.QuickCheck (quickCheck)
import Log
import LogParser

someFunc :: IO ()
someFunc = do
    parsedMessages <- testParse parse 10 "log_file.log"
    putStrLn "Parsed log messages:"
    print parsedMessages

    errorMessages <- testWhatWentWrong parse whatWentWrong "log_file.log"
    putStrLn "Error messages with severity >= 50:"
    mapM_ putStrLn errorMessages
