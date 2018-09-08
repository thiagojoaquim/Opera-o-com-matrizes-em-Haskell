--pega o N-ésimo elemento da lista

nElemento :: [Float] -> Int -> Int -> Float
nElemento [] _ _ = 0
nElemento (x:xs) n contador | contador == n = x
                            | otherwise = nElemento xs n (contador + 1)
-- pega a N-ésima coluna
pegaColuna :: [[Float]] -> Int -> [Float]
pegaColuna [] _ = []
pegaColuna (x:xs) n = [nElemento x n 0]++(pegaColuna xs n)

-- A ideia é ir pegando o N-ésimo elemento de cada coluna formando uma lista, e ligando ela
-- de tras pra frente, assim transpondo e invertendo a matriz

transp :: [[Float]] -> Int -> [[Float]]
transp [] _ = []
transp y n | ( n == length (head y)) = []
              | otherwise = (pegaColuna y n): transp y (n+ 1)

-- função rot, chama a função principal rotaciona

trans :: [[Float]] -> [[Float]]
trans [] = []
trans n = transp n 0

multiplicaEscalar :: [Float] -> [Float]-> [Float]
multiplicaEscalar [] _ =  []
multiplicaEscalar (y:ys) (x:xs) = ( x * y): multiplicaEscalar ys xs

somaLista :: [Float] -> Float
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

multiplicaLista :: [Float] -> [[Float]] -> [Float]
multiplicaLista [] _ = []
multiplicaLista _ [] = []
multiplicaLista x (y:ys) = somaLista(multiplicaEscalar x y): multiplicaLista x ys

mult2 :: [[Float]] -> [[Float]] -> [[Float]]
mult2 [] _ = []
mult2 _ [] = []
mult2 (x:xs) y = multiplicaLista x y : mult2 xs y

mult :: [[Float]] -> [[Float]] -> [[Float]]
mult [] _ = []
mult _ [] = []
mult (x:xs) (y:ys) | (length (x:xs) == length y) = mult2 (x:xs) (trans (y:ys))
                   | otherwise = []
