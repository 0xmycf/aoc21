module DayTwo.First.DayTwoFirst where

{-
    General idea
    1. Get all Lines
    2. Turn the lines into Lists 
    3. since all lines either start with some value out of ['f','u','d']
        filter those into different lists
    4. sum up all tails of all [[Char]] <=> [String] inner members
        of all three sub lists
    5. Calculate f * (d - u) <=> f * d - f * u
-}

inputPath :: String
inputPath = "DayTwo/First/input.txt"

test :: IO ()
test = do
    let a = testInputf
    let b = map last a
    print b -- "3963818188357"
    -- print $ map (read . last) testInputf
    print $ foldl (\ p c -> p + read [last c]) 0 testInputf

main :: IO ()
main = problemOne >> problemTwo

problemOne :: IO ()
problemOne = do
    lines <- getLines inputPath
    let fLines = filter (\e -> head e == 'f') lines
    let dLines = filter (\e -> head e == 'd') lines
    let uLines = filter (\e -> head e == 'u') lines
    let sumfLines = foldl (\ p c -> p + read [last c]) 0 fLines -- read expects a String and only a String, NEVER a char.
    let sumdLines = foldl (\ p c -> p + read [last c]) 0 dLines
    let sumuLines = foldl (\ p c -> p + read [last c]) 0 uLines
    print $ sumfLines * (sumdLines - sumuLines)



getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

-- a sample of possible input
testInputf :: [String]
testInputf = [
    "forward 3",
    "forward 9",
    "forward 6",
    "forward 3",
    "forward 8",
    "forward 1",
    "forward 8",
    "forward 1",
    "forward 8",
    "forward 8",
    "forward 3",
    "forward 5",
    "forward 7"
    ]

{-
Part 2

down    X increases your aim by X units.
up      X decreases your aim by X units.
forward X does two things:
    It increases your horizontal position by X units.
    It increases your depth by your aim multiplied by X.

Result:
    What do you get if you multiply your final horizontal position by your final depth?


    Flowchart
    get input   => recursive call of function that has 4 parameters
                    -> 1. The lines [String] <=> [[Char]]
                    -> 2. The aim Int
                    -> 3. The horizontal Position Int
                    -> 4. The depth
                => check line by line
                    -> if line starts with d    => aim + x
                    -> if line starts with u    => aim - x
                    -> if line starts with f    => 1. horizontal position + x
                                                   2. depth + aim * x
                    -> if no new line           => return horizontal position * depth
-}

problemTwo :: IO ()
problemTwo = do
    lines <- getLines inputPath
    print $ newCourse 0 0 0 lines
    where
        newCourse aim hp depth (x:xs)
            | head x == 'd' = newCourse (aim + read [last x]) hp depth xs
            | head x == 'u' = newCourse (aim - read [last x]) hp depth xs
            | head x == 'f' = newCourse aim (hp + read [last x]) (depth + aim * read [last x]) xs
            | otherwise     = error $ "Error while parsing " ++ x
        newCourse _ hp depth [] = hp * depth

