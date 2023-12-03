module Lib
    ( process
    ) where

import Data.Char (isDigit)
import Text.Read (readMaybe)
import InputData (day1Part1TestData, day1Part1Data, day1part2TestData, day1Part2Data)
import qualified Data.Text as T

isNumber :: Char -> [Char]
isNumber char =  [char | isDigit char]

filterDigitStringsR :: [Char] -> [Char]
filterDigitStringsR = foldr (\ c acc -> acc ++ isNumber c) ""

filterDigitStringsL :: [Char] -> [Char]
filterDigitStringsL = foldl (\ acc c -> acc ++ isNumber c) ""


takeDigitR :: Int -> [Char] -> [Char]
takeDigitR n line = take n $ filterDigitStringsR line

takeDigitL :: Int -> [Char] -> [Char]
takeDigitL n line = take n $ filterDigitStringsL line

concatFirstAndLastEachLine :: [String] -> [String]
concatFirstAndLastEachLine inputLines = map (\ line -> takeDigitL 1 line ++ takeDigitR 1 line ) inputLines

toMaybeInt :: [String] -> [Maybe Int]
toMaybeInt = map (\ x -> readMaybe x :: Maybe Int)


reduceMaybeNum :: Num a => a -> Maybe a -> a
reduceMaybeNum acc mInt =
    case mInt of
        Just n -> acc + n
        Nothing -> acc


sumMaybeNum :: (Foldable t, Num b) => t (Maybe b) -> b
sumMaybeNum = foldl reduceMaybeNum 0


process :: String -> Int
process contents = sumMaybeNum $ toMaybeInt $ concatFirstAndLastEachLine $ lines contents

-- part 2
-- leave the first and last letter of the word as they can be used in other numbers
wordToDigitConversions = [
    ("nine", "n9e"),
    ("eight", "e8t"),
    ("seven", "s7n"),
    ("six", "s6x"),
    ("five", "f5e"),
    ("four", "f4r"),
    ("three", "t3e"),
    ("two", "t2o"),
    ("one", "o1e"),
    ("zero", "z0o")]


-- reduce the text body by replacing any word numbers with the digits and first and last numbers 
replaceWordWithDigits :: T.Text -> T.Text
replaceWordWithDigits text = foldl (\ acc (x,y)-> T.replace (T.pack x) (T.pack y) acc) text wordToDigitConversions

-- Main function ghci 
-- ghci> process $ T.unpack $ replaceWordWithDigits day1Part2Data