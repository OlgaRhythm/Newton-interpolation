import System.IO
import Data.List (sort, nubBy)
import Text.Read (readMaybe)

-- Функция чтения точек из файла
readPointsFromFile :: FilePath -> IO [(Double, Double)]
readPointsFromFile filename = do
    contents <- readFile filename
    let pairs = map ((\[x, y] -> (read x, read y)) . words) (lines contents)
    return pairs

-- Удаление повторов и сортировка списка пар точек
removeDuplicatesAndSort :: [(Double, Double)] -> [(Double, Double)]
removeDuplicatesAndSort points = sort $ nubBy (\(x1, _) (x2, _) -> x1 == x2) points

-- Функция чтения степени из консоли
readDegree :: Int -> Int -> IO Int
readDegree lowerBound upperBound = do
    putStrLn $ "\nВведите степень полинома в диапазоне от " ++ show lowerBound ++ " до " ++ show upperBound ++ ":"
    input <- getLine
    case readMaybe input of
        Just number ->
            if number >= lowerBound && number <= upperBound
                then return number
                else do
                    putStrLn "Ошибка! Введите число в указанном диапазоне."
                    readDegree lowerBound upperBound
        Nothing -> do
            putStrLn "Ошибка! Введите корректное число."
            readDegree lowerBound upperBound

-- Функция чтения числа X из консоли
readNumberInRange :: Double -> Double -> IO Double
readNumberInRange lowerBound upperBound = do
    putStrLn $ "\nВведите число в диапазоне от " ++ show lowerBound ++ " до " ++ show upperBound ++ ":"
    input <- getLine
    case readMaybe input of
        Just number ->
            if number >= lowerBound && number <= upperBound
                then return number
                else do
                    putStrLn "Ошибка! Введите число в указанном диапазоне."
                    readNumberInRange lowerBound upperBound
        Nothing -> do
            putStrLn "Ошибка! Введите корректное число."
            readNumberInRange lowerBound upperBound

-- Функция для вычисления разделенной разности
-- i - строка
-- j - столбец
dividedDifferences :: [Double] -> [Double] -> Int -> Int -> Double
dividedDifferences _ ys 0 0 = head ys
dividedDifferences xs ys i 0 = dividedDifferences (tail xs) (tail ys) (i-1) 0
dividedDifferences xs ys i j = (y2 - y1) / (xs !! (j + i) - xs !! i)
    where 
        y2 = dividedDifferences xs ys (i + 1) (j - 1)
        y1 = dividedDifferences xs ys i (j - 1)

-- Функция для вычисления произведения (x - x0)(x - x1)..
term :: [Double] -> Double -> Int -> Double
term xs x 0 = x - (head xs) 
term xs x degree = (x - head xs) * (term (tail xs) x (degree - 1)) 

-- Функция интерполяции методом Ньютона
newtonInterpolation :: [Double] -> [Double] -> Int -> Double -> Double
newtonInterpolation xs ys 0 targetX = head ys
newtonInterpolation xs ys degree targetX = result
    where
        result = (dividedDifferences xs ys 0 degree) * (term xs targetX (degree-1)) + (newtonInterpolation xs ys (degree-1) targetX)
        
-- Функция для красивого вывода точек
writePoints :: [Double] -> [Double] -> IO()
writePoints [] [] = return ()
writePoints xs ys = do
    putStrLn $ show (head xs) ++ " \t" ++ show (head ys)
    writePoints (tail xs) (tail ys)


main :: IO ()
main = do
    putStrLn "Интерполяция методом Ньютона"

    -- Чтение точек из файла
    points <- readPointsFromFile "input.txt"

    -- Удаление повторов и сортируем
    let uniqueSortedPoints = removeDuplicatesAndSort points

    -- Разделение точек на два списка
    let xPoints = map fst uniqueSortedPoints
        yPoints = map snd uniqueSortedPoints

    putStrLn "Даны пары точек:"
    writePoints xPoints yPoints
    
    -- Чтение степени полинома
    degree <- readDegree 1 ((length xPoints)-1)

    -- Чтение значения Х
    targetX <- readNumberInRange (xPoints !! 0) (xPoints !! degree)

    -- Вычисление значения интерполяции
    let interpolationResult = newtonInterpolation xPoints yPoints degree targetX
    putStrLn $ "x = " ++ show targetX ++ "\ny = " ++ show interpolationResult