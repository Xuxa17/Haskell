 {-# LANGUAGE LambdaCase #-}
module Lib (parse, makeDict, makeSentence, goDialog) where

import Data.List (dropWhileEnd)
import Data.Map (Map, empty, insertWith, lookup, member, toAscList, fromListWith, unionWith) 
import Data.Char (isAlpha, isSpace, toLower)
import System.IO
import Data.List (tails, nub, intercalate)
import Data.List.Split (splitOn, splitOneOf)
import System.Random (randomRIO)


-- фильтруем текст, чтобы не было ничего лишнего

filterText :: Char -> Char
filterText ch
    | isAlpha ch || isSpace ch || ch `elem` "'.!?;:()" = ch
    | otherwise = ' '

-- разделяем на предложения по оставшимся символам
splitSentence :: String -> [String]
splitSentence str = splitOneOf ".!?;:()" str

-- теперь всё разделяем на предложения
parseText :: String -> [[String]]
parseText text = filter (not . null) . map words . splitSentence $ (map filterText text1) 
    where text1 = map toLower text

-- Чтение текста из файла
getText :: FilePath -> IO String
getText path = readFile path

-- Сохранение предложений в файл
sentencesToFile :: FilePath -> [[String]] -> IO ()
sentencesToFile path sentences =
    openFile path WriteMode >>= \handle ->
        mapM_ (hPutStrLn handle . unwords) sentences >>
        hClose handle

parse :: IO()
parse = 
    putStrLn "Text from file:" >>
    hFlush stdout >>
    getLine >>= \file1 ->
    putStrLn "Sentences to file" >>
    hFlush stdout >>
    getLine >>= \file2 ->
    getText file1 >>= \str ->
    let sentences = parseText str
    in sentencesToFile file2 sentences


-- ####################################################################################################################################
makeNGram :: [[String]] -> Map String [String]
makeNGram parsedText = foldr (sentenceToNGram) empty parsedText


sentenceToNGram :: [String] -> Map String [String] -> Map String [String]
sentenceToNGram sentence gram =
    let bigrams' = generateBigrams sentence
        trigrams' = generateTrigrams sentence
    in foldr addBigram (foldr addTrigram gram trigrams') bigrams'


-- Функция для генерации биграмм (пары слов)
generateBigrams :: [String] -> [(String, String)]
generateBigrams words1 = [(w1, w2) | (w1:w2:_) <- tails words1] --берем первые 2 элемента из каждого хвоста списка (у нас все возможные хвосты)

-- Функция для генерации триграмм (тройки слов)
generateTrigrams :: [String] -> [(String, String, String)]
generateTrigrams words1 = [(w1, w2, w3) | (w1:w2:w3:_) <- tails words1]
 
removeSpace :: String -> String
removeSpace = f . f
  where f = reverse . dropWhileEnd (== ' ')

addUnique :: [String] -> [String] -> [String]
addUnique newVal existingVals = nub $ newVal ++ existingVals -- убираем повторы

addBigram :: (String, String) -> Map String [String] -> Map String [String]
addBigram (a, b) nGram = insertWith addUnique a [b] nGram -- insertWith вставляет пару по ключу a

addTrigram :: (String, String, String) -> Map String [String] -> Map String [String]
addTrigram (a, b, c) nGram = 
    insertWith addUnique (removeSpace $ a ++ " " ++ b) [removeSpace c] $
    insertWith addUnique (removeSpace a) [removeSpace $ b ++ " " ++ c] nGram


saveDict :: FilePath -> Map String [String] -> IO ()
saveDict filePath dict =
    let into = Data.Map.toAscList dict
        filterInto = filter (not . null . snd) into  -- проверяем чтобы второй элемент не был равен нулю
        readyInto = map myFormat filterInto
    in writeFile filePath (unlines readyInto)
  where
    myFormat (key, values) =
      let values' = nub values
          notEmpty = if all null values' then [] else filter (not . null) values'
          valuesStr = intercalate ", " (map show notEmpty) -- объединяет всё в один список через запятую
      in key ++ " : " ++ valuesStr


makeDict :: IO ()
makeDict =
    putStrLn "Text from file:" >>
    hFlush stdout >>
    getLine >>= \file1 ->
    putStrLn "Dictionary to file" >>
    hFlush stdout >>
    getLine >>= \file2 ->
    getText file1 >>= \str ->
    let
        sentences = parseText str
        dict = makeNGram sentences
        dictFileName = file2
    in saveDict dictFileName dict 

-- ####################################################################################################################################

-- Основная функция для генерации фразы
generatePhrase :: Map String [String] -> String -> IO String
generatePhrase dict startWord = 
  case Data.Map.lookup startWord dict of
    Nothing -> return "Not in dictionary"
    Just words1 -> generatePhraseRec dict words1 14 (startWord ++ " ") >>= return . unwords

splitWord :: String -> [String]
splitWord str = splitOn " " str

--Рекурсивная функция для генерации фразы заданной длины
generatePhraseRec :: Map String [String] -> [String] -> Int -> String -> IO [String]
generatePhraseRec _ _ 0 _ = return []
generatePhraseRec dict currentWords remainingLength key =
    let len = length currentWords
        indexRange = (0, len - 1)  -- Определяем диапазон для генерации индекса
    in randomRIO indexRange >>= \index ->
       let word = currentWords !! index
           word1 = key ++ "     " ++ word
           wordLength = splitWord word
       in case Data.Map.lookup word dict of
            Nothing -> return [word]  -- Если нет продолжений, возвращаем текущее слово
            Just nextWords -> 
                generatePhraseRec dict nextWords (remainingLength - 1) "" >>= \restOfPhrase ->                                                           -- (length wordLength)
                return (word1 : restOfPhrase)  -- Соединяем текущее слово с рекурсивным результатом


makeSentence :: IO ()
makeSentence =
    putStrLn "Text from file: " >>
    hFlush stdout >>
    getLine >>= \filename ->
    readFile filename >>= \content ->
    let sentences = parseText content 
        dict = makeNGram sentences
    in putStrLn "Key word: " >>
            hFlush stdout >>
            getLine >>= \input ->
            let keyWords = words input
                key = unwords keyWords
            in case Data.Map.lookup key dict of
                Nothing -> putStrLn ("There is no key: '" ++ key)
                Just _ -> generatePhrase dict key >>= \case
                    err -> putStrLn err
                    phrase -> putStrLn ("Result: " ++ phrase)


-- ####################################################################################################################################

dialog :: Map String [String] -> Map String [String] -> String -> Int -> IO ()
dialog dict1 dict2 initialPhrase steps = makeDialog initialPhrase dict1 dict2 steps 1
    
makeDialog :: String -> Map String [String] -> Map String [String] -> Int -> Int -> IO()
makeDialog prevPhrase d1 d2 remainingSteps personNumber
            | remainingSteps == 0 = putStrLn "Dialog is over"
            | otherwise =
                let currentP = personNumber `mod` 2 == 1
                    dict = if (currentP == True) then d1 else d2 
                    otherDict = if (currentP == True) then d2 else d1
                    player_ind = fromEnum currentP
                    playerLabel = "Person " ++ show player_ind ++ ": "
                in generatePhrase dict prevPhrase >>= \case
                    generatedPhrase ->
                        let lastWords = reverse (words generatedPhrase)
                        in putStrLn (playerLabel ++ generatedPhrase) >>
                            case findLastKey lastWords otherDict of
                            Nothing -> putStrLn "There is no such key"
                            Just nextKey -> 
                                makeDialog nextKey d1 d2 (remainingSteps - 1) ((personNumber + 1) `mod` 2) 

-- передаем (уже перевернутое) предложение и смотрим есть ли первый элемент в словаре, ну и потом его возвращаем
findLastKey :: [String] -> Map String [String] -> Maybe String
findLastKey [] _ = Nothing
findLastKey (x:xs) dict
    | Data.Map.member x dict = Just x  
    | otherwise = findLastKey xs dict  


goDialog :: IO ()
goDialog =
    putStrLn "First file: " >>
    hFlush stdout >>
    getLine >>= \file1 ->
    putStrLn "Second file: " >>
    hFlush stdout >>
    getLine >>= \file2 ->
    readFile file1 >>= \content1 ->
    readFile file2 >>= \content2 ->
    case (parseText content1, parseText content2) of
        (sentences1, sentences2) ->
            let 
                dict1 = makeNGram sentences1
                dict2 = makeNGram sentences2
            in putStrLn "Key: " >>
               hFlush stdout >>
               getLine >>= \input ->
               let initialKey = words input
               in putStrLn "Depth: " >>
                  hFlush stdout >>
                  getLine >>= \mInput ->
                  let m = read mInput :: Int
                  in dialog dict1 dict2 (unwords initialKey) m

