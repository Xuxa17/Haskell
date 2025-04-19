import Lib(isCongruent, filterEvenOrOdd)

main :: IO ()
main = do
    let a = isCongruent (-12) 3 5
    print a
    let numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let num = []
    print (filterEvenOrOdd numbers True)
    print (filterEvenOrOdd numbers False)
    print (filterEvenOrOdd num False)

