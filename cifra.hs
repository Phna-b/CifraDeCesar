import Data.Char (chr, ord, isUpper, isLower) -- Biblioteca que utilizei para facilitar o processo de validação dos caracteres (Converte ASCII para char, Converte char para ASCII, verifica maisuculas/minusculas )
 
cifra num letra =    -- Função da Cifra, considerando letras maisuculas, minusculas e outros casos.
    if isUpper letra 
        then chr ( ord 'A' + mod (ord letra - ord 'A' + num) 26 ) -- Utilizando ORD pra transformar em ASCII e mod 26 para garantir que não escape do alfabeto,
                                                                  -- calculei a posição da letra baseado no A e somei a chave para realizar o novo calculo
    else if isLower letra 
        then chr ( ord 'a' + mod (ord letra - ord 'a' + num) 26 )
    else letra

crip l msg = map (cifra l) msg
desc l msg = crip (-l) msg -- Como o tipo de criptofrafia que estamos usando é simetrico, só foi necessário inverter a chave para descriptografar. 

uiCrip = do -- Função de UI para realizar criptografia, coleta as informações do usuário e retorna para exeCifra
    putStrLn "Qual a palavra que deseja criptografar?"
    msg <- getLine
    putStrLn "Digite o numero de deslocamento:"
    des <- getLine
    let desL = read des :: Int -- Converter char para int
    let criptografada = crip desL msg
    putStrLn $ "Mensagem criptografada: " ++ criptografada

uiDesc = do -- Função de UI para realizar descriptografar, coleta as informações do usuário e retorna para exeCifra
    putStrLn "Qual a palavra que deseja descriptografar?"
    msg <- getLine
    putStrLn "Digite o numero de deslocamento:"
    des <- getLine
    let desL = read des :: Int -- Converter char para int
    let descriptografada = desc desL msg
    putStrLn $ "Mensagem descriptografada: " ++ descriptografada

exeCifra = do -- "Main", console para usuário realizar as ações 
    putStrLn "Qual operação deseja realizar?"
    putStrLn "1 - Criptografar;"
    putStrLn "2 - Descriptografar."
    msg <- getLine
    let msg1 = read msg :: Int
    if msg1 == 1 then uiCrip
    else if msg1 == 2 then uiDesc
    else  exeCifra


