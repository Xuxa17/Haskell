data Move = Cooperate | Defect deriving (Eq, Show)
roundPeriod :: Move -> Move -> (Int, Int)


-- если играем на срок заключения => победа у того, у кого минимум, и в таблице выбираем у нас минимум, а у противника максимум
roundPeriod Cooperate Cooperate = (1, 1) -- Оба молчат
roundPeriod Cooperate Defect = (10, 0) -- Один молчит, другой предает
roundPeriod Defect Cooperate = (0, 10) -- Один предает, другой молчит 
roundPeriod Defect Defect = (2, 2)-- Оба предают


eyeForAnEye :: Int -> [Move] -> Move
eyeForAnEye counter moves
    | counter == 0 = Cooperate
    | otherwise = head moves

playEyeForAnEye :: Int -> [Move] -> [Move] 
playEyeForAnEye counter userMoves  =
    let compMove = eyeForAnEye counter userMoves
        olduserMoves = if counter == 0 then userMoves else tail userMoves
    in compMove : playEyeForAnEye (counter+1) olduserMoves


--Ходы компьютера для равновесия по Нэшу
getStrategy :: (Int, Int) -> Move
getStrategy s = if s == (0, 10) || s == (2, 2)
                 then Defect
                 else Cooperate

playNashStrategy :: Int -> [Move]
playNashStrategy numberMoves =
    let matrix = [(1, 1), (10, 0), (0, 10), (2, 2)]
        compNashMoves = replicate numberMoves (getStrategy (minimum (map fst matrix), maximum (map snd matrix))) 
    in compNashMoves

--Определение победителя
getPoints :: (a1, a2, (a3, b4)) -> (a3, b4)
getPoints (_, _, (a3, b4)) = (a3, b4)

whoWin :: [(Int, (Move, Move, (Int, Int)))] -> IO() 
whoWin roundsWithNumbers = do
    let lastRound = last roundsWithNumbers --выбираем последний раунд
    let (compScore, userScore) = getPoints (snd lastRound) --из представления раунда берем часть, которая после номера раунда, а потом при помощи функции getPoints получаем очки, которые там получили
    let minScore = min compScore userScore -- выбираем минимальные баллы, тк для победы у человека должно быть как можно меньше баллов, это когда играем на срок
    if compScore == userScore then
        putStrLn("Ничья!" ++ "   Срок пользователя: " ++ show (userScore) ++ "   Срок компьтера: " ++ show (compScore))
    else if minScore == compScore then
        putStrLn ("Победил компьютер!" ++ "   Срок пользователя: " ++ show (userScore) ++ "   Срок компьтера: " ++ show (compScore))
    else
        putStrLn ("Победил пользователь!" ++ "   Срок пользователя: " ++ show (userScore) ++ "   Срок компьтера: " ++ show (compScore))



--Реализация игры
--Суммирование очков
mySum :: (Int, Int) -> (Int, Int) -> (Int, Int)
mySum (one1 , two1) (one2 , two2) = (one1 + one2 , two1 + two2)

--Вывод результата для каждого раунда
printRes :: (Int, (Move, Move, (Int, Int))) -> IO ()
printRes (roundNumber, (opponentMove, userMove, points)) = putStrLn("Раунд " ++ show roundNumber ++ "   Компьютер : "
         ++ show opponentMove ++ "   Пользователь: " ++ show userMove ++ "   Текущий срок: " ++ show points)

playGame :: [Move] -> [Move] -> IO ()
playGame compMoves userMoves = do 
    let gameResults = zipWith roundPeriod compMoves userMoves -- в каждом раунде определяем кто, сколько набирает
    let newRes = roundPeriod (head compMoves) (head userMoves)
    let sumPoints = scanl mySum (newRes) (tail gameResults) -- суммируем баллы, которые получили раньше, с теми, которые получили позже
    let roundsWithMoves = zip3 compMoves userMoves sumPoints -- соединяем шаги обоих игроков и полученные баллы в лист
    let roundsWithNumbers = zip [1..] roundsWithMoves -- соединияем в лист номер шага и полученные тройки
    mapM_ printRes roundsWithNumbers
    whoWin roundsWithNumbers



--Ходы пользователя
userMoves0 :: [Move]
userMoves0 = [Defect, Defect, Defect, Cooperate, Defect, Defect, Cooperate, Defect, Cooperate, Defect, Defect]

userMoves1 :: [Move]
userMoves1 = [Cooperate, Cooperate, Cooperate, Cooperate, Cooperate, Cooperate, Cooperate, Cooperate, Cooperate, Cooperate]

main :: IO ()
main = do
    let compMovesEyes = playEyeForAnEye 0 userMoves0
    let compMovesNash = playNashStrategy (length userMoves0)
    putStrLn "Стратегия 'Око за око':"
    playGame (take 100 compMovesEyes) (take 100 userMoves0)
    putStrLn "Стратегия равновесия по Нэшу:"
    playGame (take 100 compMovesNash) (take 100 userMoves0)
    let compMovesEyes1 = playEyeForAnEye 0 userMoves1
    putStrLn "Стратегия 'Око за око', когда будет наибольший совместный выигрыш"
    playGame (take 100 compMovesEyes1) (take 100 userMoves1)
    



{-
--если играем на деньги => победа у того, у кого максимум
roundPeriod Cooperate Defect = (-100, 500) -- Один молчит, другой предает
roundPeriod Defect Cooperate = (500, -100) -- Один предает, другой молчит
roundPeriod Cooperate Cooperate = (300, 300) -- Оба молчат
roundPeriod Defect Defect = (-10, -10)-- Оба предают
-}

{-
--Стратегия око за око
eyeForAnEye :: [Move] -> Move
eyeForAnEye [] = Cooperate  -- Первый ход - сотрудничество
--eyeForAnEye moves = head (reverse(moves)) -- take (n - 1) а здесь в конце не забываем сделать всё реверс
eyeForAnEye moves = head moves -- drop(n) и не забыть reverse изначальный список -- Затем повторяем ход противника

playEyeForAnEye :: Int -> [Move] -> [Move] -> [Move] 
playEyeForAnEye 0 _ _ = []
playEyeForAnEye numberMoves compMoves playerMoves = let compMove = eyeForAnEye (drop(numberMoves) playerMoves)
    in compMove : playEyeForAnEye (numberMoves - 1) (compMove : compMoves) playerMoves
-}

{-
--Ходы компьютера для равновесия по Нэшу
getStrategy :: (Int) -> Move
getStrategy (comp) = if comp == 0
                     then Defect
                     else Cooperate

playNashStrategy :: Int -> [Move]
playNashStrategy numberMoves =
    let comp_years = [1, 10, 0, 2]
        compNashMoves = replicate numberMoves (getStrategy (minimum (comp_years))) 
    in compNashMoves
-}





