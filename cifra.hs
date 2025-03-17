import Data.Char (chr, ord, isAlpha, isUpper, isLower)

cifra :: Int -> Char -> Char
cifra num letra 
    | isUpper letra = chr $ ord 'A' + mod (ord letra - ord 'A' + num) 26
    | isLower letra = chr $ ord 'a' + mod (ord letra - ord 'a' + num) 26
    | otherwise = letra


enc l = map (cifra l) 


dec l = enc (-l)

main :: IO ()
main = do
    putStrLn "Qual a palavra que deseja encriptar?"
    msg <- getLine
    putStrLn "Digite o numero de deslocamento:"
    des <- getLine
    let desL = read des :: Int -- Converter char para int
    let criptografada = enc desL msg
    let descriptografada = dec desL criptografada
    putStrLn $ "Mensagem descriptografada: " ++ descriptografada ++ " / Mensagem criptografada: " ++ criptografada



 
 



