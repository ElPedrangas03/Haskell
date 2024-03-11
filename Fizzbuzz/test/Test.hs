module Main where

import Test.Tasty
import Test.Tasty.HUnit
import MyLib

lessThan20Answers = words ("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")

tensAnswers = words ("twenty thirty forty fifty sixty seventy eighty ninety")


fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 3 == 0 && n `mod` 5 == 0 = "fizzbuzz!"
    | n `mod` 3 == 0 = "fizz!"
    | n `mod` 5 == 0 = "buzz!"
    | otherwise = number n ++ "!"

-- Metodo que recibe un entero y regresa su equivalente en texto, esto solo para los numeros de 1 a 20
-- Entrada: 4
-- Salida: four
lessThan20 :: Int -> String
lessThan20 n |
    n > 0 && n < 20 =
    let answers = words ("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
    in answers !! (n-1)

-- Metodo que recibe un entero y regresa el valor de cada 10
-- Entrada: 20
-- Salida: twenty
tens :: Int -> String
tens n | n >= 2 && n <= 9 =
    answers !! (n-2)
    where
        answers = words("twenty thirty forty fifty sixty seventy eighty ninety")

-- Metodo general para calculas el FizzBuzz
-- Entrada: 15
-- Salida: FizzBuzz
number :: Int -> String
number n 
    | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz" 
    | n `mod` 3 == 0 = "Fizz"
    | n `mod` 5 == 0 = "Buzz"
    | 1 <= n && n<20 = lessThan20(n) 
    | n `mod` 10 == 0 && n<100=tens (n `div` 10)
    | n < 100 = tens(n `div` 10) ++ " " ++ lessThan20(n `mod` 10)

fizzBuzzSuite :: TestTree
fizzBuzzSuite = testGroup "FizzBuzz tests"
                [ testGroup "fizzbuzz" $
                    [ testCase "1 is one!" $ fizzbuzz 1 @?= "one!"
                    , testCase "2 is two!" $ fizzbuzz 2 @?= "two!"
                    , testCase "3 is fizz!" $ fizzbuzz 3 @?= "fizz!"
                    , testCase "4 is four!" $ fizzbuzz 4 @?= "four!"
                    , testCase "5 is buzz!" $ fizzbuzz 5 @?= "buzz!"
                    , testCase "15 is fizzbuzz!" $ fizzbuzz 15 @?= "fizzbuzz!"
                    , testCase "18 is fizz!" $ fizzbuzz 18 @?= "fizz!"
                    , testCase "22 is twenty two!" $ fizzbuzz 22 @?= "twenty two!"
                    , testCase "25 is buzz!" $ fizzbuzz 25 @?= "buzz!"
                    , testCase "60 is fizzbuzz!" $ fizzbuzz 60 @?= "fizzbuzz!"
                    , testCase "99 is fizz!" $ fizzbuzz 99 @?= "fizz!"
                    , testCase "100 is buzz!" $ fizzbuzz 100 @?= "buzz!"
                    ]
                , testGroup "lessThan20" $
                    map (\(n, t) -> testCase (show n ++ " is " ++ t) $ lessThan20 n @?= t)
                        (zip [1..] lessThan20Answers)
                , testGroup "tens" $
                    map (\(n, t) -> testCase (show n ++ " is " ++ t) $ tens n @?= t)
                    (zip [2..] tensAnswers)
                , testGroup "number"
                    [ testCase "1 is one" $ number 1 @?= "one"
                    , testCase "5 is Buzz" $ number 5 @?= "Buzz"
                    , testCase "10 is Buzz" $ number 10 @?= "Buzz"
                    , testCase "11 is eleven" $ number 11 @?= "eleven"
                    , testCase "19 is nineteen" $ number 19 @?= "nineteen"
                    , testCase "20 is Buzz" $ number 20 @?= "Buzz"
                    , testCase "25 is Buzz" $ number 25 @?= "Buzz"
                    , testCase "50 is Buzz" $ number 50 @?= "Buzz"
                    , testCase "59 is fifty nine" $ number 59 @?= "fifty nine"
                    , testCase "90 is FizzBuzz" $ number 90 @?= "FizzBuzz"
                    , testCase "91 is ninety one" $ number 91 @?= "ninety one"
                    , testCase "99 is Fizz" $ number 99 @?= "Fizz"
                    , testCase "100 is Buzz" $ number 100 @?= "one hundred"
                    ]
                ]


main :: IO ()
main = do
    putStrLn "Running FizzBuzz tests..."
    defaultMain fizzBuzzSuite
