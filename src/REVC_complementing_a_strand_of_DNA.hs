-- ID:      REVC
-- Name:    Complementing a Strand of DNA
-- Author:  Samuel Jackson
-- Email:   samueljackson@outlook.com

complementDNA :: String -> String
complementDNA xs = reverse $ map complementNucleotide xs
  where
    complementNucleotide :: Char -> Char
    complementNucleotide x
      | x == 'A' = 'T'
      | x == 'T' = 'A'
      | x == 'G' = 'C'
      | x == 'C' = 'G'
      | otherwise = error "complementDNA: Unknown Nucleotide in string"

main:: IO ()
main = do
    line <- getLine
    putStrLn $ complementDNA line