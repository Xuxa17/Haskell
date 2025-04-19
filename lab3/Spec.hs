import Lib(isCongruent, filterEvenOrOdd)
import Test.QuickCheck

prop_isCongruent1 :: Int -> Int -> Int -> Bool
prop_isCongruent1 a b c = 
    if c == 0 then isCongruent a b c == False 
    else isCongruent a b c == (mod (a-b) c == 0)


prop_isCongruent2 :: Int -> Int -> Int -> Bool
prop_isCongruent2 a b c = isCongruent a b c == isCongruent b a c


prop_isCongruent3 :: Int -> Int -> Bool
prop_isCongruent3 a c = 
    if c == 0 then isCongruent a a c == False
    else  isCongruent a a c == True


prop_filterEvenOrOdd1 :: [Int] -> Bool
prop_filterEvenOrOdd1 s = filterEvenOrOdd s True == filter (\x -> x `mod` 2 == 0) (filterEvenOrOdd s True)


prop_filterEvenOrOdd2 :: [Int] -> Bool
prop_filterEvenOrOdd2 s = filterEvenOrOdd s False == filter (\x -> x `mod` 2 == 1) (filterEvenOrOdd s False)


prop_filterEvenOrOdd3 :: [Int] -> Bool -> Bool
prop_filterEvenOrOdd3 xs i = (length(filterEvenOrOdd xs i) <= length xs)


prop_filterEvenOrOdd4 :: Bool -> Bool
prop_filterEvenOrOdd4 i =  filterEvenOrOdd [] i == []


main :: IO ()
main = do
    putStrLn("Если два числа равны по модулю, то их разность делится на модуль")
    quickCheckWith stdArgs {maxSuccess = 10000}
        prop_isCongruent1
    --quickCheck prop_isCongruent1
    putStrLn("Равенство по модулю является симметричным")
    quickCheckWith stdArgs {maxSuccess = 10000}
         prop_isCongruent2
    putStrLn("Если одно число равно другому, то они равны по любому модулю")
    quickCheckWith stdArgs {maxSuccess = 10000}
        prop_isCongruent3
    putStrLn(" Все элементы результата являются четными при True")
    quickCheckWith stdArgs {maxSuccess = 10000}
         prop_filterEvenOrOdd1
    putStrLn("Все элементы результата являются нечетными при False")
    quickCheckWith stdArgs {maxSuccess = 10000}
         prop_filterEvenOrOdd2
    putStrLn("Длина результата не превышает длину исходного списка")
    quickCheckWith stdArgs {maxSuccess = 10000}
         prop_filterEvenOrOdd3
    putStrLn("Если исходный список пустой, то результат также пустой")
    quickCheckWith stdArgs {maxSuccess = 10000}
         prop_filterEvenOrOdd4