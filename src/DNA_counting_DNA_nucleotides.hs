-- ID:      DNA
-- Name:    Counting DNA Nucleotides
-- Author:  Samuel Jackson
-- Email:   samueljackson@outlook.com

import Data.List

--count the nucleotides in a string representation e.g. AAATTGGCCATGCCC
countNucleotides :: String -> String
countNucleotides strs =  convertToString $ map (countElem strs) "ACGT"
  where
    --count the occurance of each element in a string
    countElem :: String -> Char -> Int
    countElem xs elm = length $ filter (elm==) xs

    --convert a list of ints to a string
    convertToString :: [Int] -> String
    convertToString xs = intercalate " " (map show xs)

main:: IO ()
main = do
    line <- getLine
    putStrLn $ countNucleotides line