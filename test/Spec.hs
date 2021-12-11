
main :: IO ()
main = print "no tests implemented"



{-

test :: [Int]
test = [2,4,6,5]
-- 2,4,6=12, 4,6,5=15

countIncreasesPart2 :: [Int] -> IO Int
countIncreasesPart2 xs = do
    print (zip xs (drop 3 xs))
    return $ length (filter (== True) (zipWith (<) xs (drop 3 xs)))
    -}