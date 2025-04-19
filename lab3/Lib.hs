module Lib(isCongruent, filterEvenOrOdd) where

--два числа равны по модулю третьего, когда разность этих чисел делится на третье число
--ну или на прямую сравнивать остатки
isCongruent :: Int -> Int -> Int -> Bool
isCongruent a b c = 
    if c == 0 then False --вообще они не могут быть равны, потому что деление на 0 не определено
    else (b1 == b1)
        where
            a1 = a `mod` c 
            b1 = b `mod` c 


filterEvenOrOdd :: [Int] -> Bool -> [Int]
filterEvenOrOdd s i = 
    if i == True
    then filter odd s -- [x | x <- s, even x]
    else filter odd s 