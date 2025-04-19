type Point = (Double, Double)

-- Функция для вычисления новых точек
kochCurve :: Int -> Point -> Point -> [Point]
kochCurve 0 p1 p2 = [p1, p2]

kochCurve n p1 p2 =
    let (x1, y1) = p1
        (x2, y2) = p2
        dx = (x2 - x1) / 3 --находим длину одного маленького отрезка
        dy = (y2 - y1) / 3
        pA = (x1 + dx, y1 + dy) -- координаты точек, разделяющих отрезок на 4 части
        pB = (x1 + 2 * dx, y1 + 2 * dy)
        peak = ( (x1 + x2) / 2 + (sqrt 3 / 6) * (y1 - y2),
               (y1 + y2) / 2 + (sqrt 3 / 6) * (x2 - x1) )
        line1 = kochCurve (n - 1) p1 pA
        line2 = kochCurve (n - 1) pA peak
        line3 = kochCurve (n - 1) peak pB
        line4 = kochCurve (n - 1) pB p2
    in
        line1 ++ tail line2 ++ tail line3 ++ tail line4


allKoch :: Int -> Point -> Point -> [[Point]]
allKoch n p1 p2 =  concatMap(\i -> [kochCurve i p1 p2]) [0..n]


-- Пример использования
main :: IO ()
main = do
    let depth = 2
    let start = (0, 0)
    let end = (3, 0)
    let res = allKoch depth start end
    print res
