-- ID:      RNA
-- Name:    Transcribing DNA into RNA
-- Author:  Samuel Jackson
-- Email:   samueljackson@outlook.com

--replace any occurance of T with the letter U
transcribe :: String -> String
transcribe xs = map replace xs
  where
    replace :: Char -> Char
    replace x
      | x == 'T' = 'U'
      | otherwise = x

main:: IO ()
main = do
    line <- getLine
    putStrLn $ transcribe line