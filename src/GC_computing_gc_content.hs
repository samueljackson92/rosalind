-- ID:      GC
-- Name:    Computing GC Content
-- Author:  Samuel Jackson
-- Email:   samueljackson@outlook.com

import System.IO
import System.Environment

import Data.Char
import Data.List
import Data.List.Split
import Data.Ord

type DNAString = (String, String)
type PercentagePair = (String, Float)

main = do
  --parse command line arguments
  args <- getArgs
  let fileName = head args

  --open file name and read in contents
  withFile fileName ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr $ computeGCContent contents)

--parse a file in the FASTA format and return ID of string with highest GC content
computeGCContent :: String -> String
computeGCContent contents = formatAsString $ findMaxPair
                              $ map convertToPercentage $ parseFASTAFormat contents
  where
    --convert an PercentagePair to a string
    formatAsString :: PercentagePair -> String
    formatAsString (name, percent) = name ++ "\n" ++ show percent ++ "\n"

    --find the maximum pair in a list of pairs
    findMaxPair :: [PercentagePair] -> PercentagePair
    findMaxPair pairs = maximumBy (comparing snd) pairs

    --convert the DNA string to a percentage of it's GC count
    convertToPercentage :: DNAString -> PercentagePair
    convertToPercentage (id, dna) = (id, (gcPercentage dna))

    --compute percentage of G and C characters in DNA string
    gcPercentage :: String -> Float
    gcPercentage dna = ((countChars 'G' dna) + (countChars 'C' dna))
                        / (fromIntegral $ length dna) *100
      where
        countChars :: Char -> String -> Float
        countChars c xs = fromIntegral $ length $ filter (==c) xs

--Strip file into id and DNA string pairs
parseFASTAFormat :: String -> [DNAString]
parseFASTAFormat contents = map removeNewLines $ splitContents contents
  where
    splitContents :: String -> [DNAString]
    splitContents contents = map (break (=='\n')) $ tail $ splitOn ">" contents

    removeNewLines :: (String, String) -> (String, String)
    removeNewLines (name, seq) = (name, (filter (/= '\n') seq))