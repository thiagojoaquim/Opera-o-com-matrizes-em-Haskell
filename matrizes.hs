--pega o N-ésimo elemento da lista

nElemento :: [Int] -> Int -> Int -> Int
nElemento [] _ _ = 0
nElemento (x:xs) n contador | contador == n = x
                            | otherwise = nElemento xs n (contador + 1)
-- pega a N-ésima coluna
pegaColuna :: [[Int]] -> Int -> [Int]
pegaColuna [] _ = []
pegaColuna (x:xs) n = [nElemento x n 0]++(pegaColuna xs n)

-- A ideia é ir pegando o N-ésimo elemento de cada coluna formando uma lista, e ligando ela
-- de tras pra frente, assim transpondo e invertendo a matriz

transp :: [[Int]] -> Int -> [[Int]]
transp [] _ = []
transp y n | ( n == length (head y)) = []
              | otherwise = (pegaColuna y n): transp y (n+ 1)

-- função rot, chama a função principal rotaciona

trans :: [[Int]] -> [[Int]]
trans [] = []
trans n = rotaciona n 0
