module Main (main) where

import Lib (parse, makeDict, makeSentence, goDialog)

main :: IO ()
main = 
    putStrLn "Task number: " >>
    getLine >>= \number ->
    if number == "1"
    then parse >> main
    else if number == "2"
    then makeDict >> main
    else if number == "3"
    then makeSentence >> main
    else if number == "4"
    then goDialog >> main
    else putStrLn("error")

