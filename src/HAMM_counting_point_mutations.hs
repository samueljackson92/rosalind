-- ID:      GC
-- Name:    Counting Point Mutations
-- Author:  Samuel Jackson
-- Email:   samueljackson@outlook.com

--compute the hamming distance between two strings
hammingDistance :: String -> String -> Int
hammingDistance s t = length . filter (\(x, y) -> x/=y) $ zip s t
