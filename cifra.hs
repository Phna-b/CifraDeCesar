import Data.Char (chr, ord, isAlpha, isUpper, isLower)

cifra :: Int -> Char -> Char
cifra num letra 
    | isUpper letra = chr $ ord 'A' + mod (ord letra - ord 'A' + num) 26
    | isLower letra = chr $ ord 'a' + mod (ord letra - ord 'a' + num) 26
    | otherwise = letra



 