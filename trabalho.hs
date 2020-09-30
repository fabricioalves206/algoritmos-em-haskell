--funcao de ackermann
a :: Int->Int->Int
a m n
        |m == 0 = n + 1
        |m > 0 && n == 0 = a (m-1) 1
        |m > 0 && n >0 = a (m-1) (a m (n-1))



-- lista odernada de elementes exclusivos de duas listas distintas
exclusivo :: [Int]->[Int]->[Int]
exclusivo x y = exclusivoAux (qsort x) (qsort y)

qsort :: [Int]->[Int]
qsort [] = []
qsort (cab:cauda) = qsort [ y | y <- cauda, y < cab]
                      ++ [cab]
                      ++ qsort [y | y <- cauda, y >=  cab]

exclusivoAux :: [Int]->[Int]->[Int]
exclusivoAux (h1:t1) (h2:t2)
                            |h1 > h2 = [h2] ++ exclusivoAux (h1:t1) t2
                            |h1 < h2 = [h1] ++ exclusivoAux t1 (h2:t2)
                            |h1 == h2 = exclusivoAux t1 t2

exclusivoAux [] [] = []
exclusivoAux [] (h2:t2) = [h2] ++ exclusivo [] t2
exclusivoAux (h1:t1) [] = [h1] ++ exclusivo t1 []

--nsimo elemento de uma lista de inteiros
nEsimo :: Int->[Int]->Int
nEsimo x (cab:cauda)
                    |x > 1 = nEsimo (x-1) cauda
                    |otherwise = cab

-- insere um elemento na nsima posição
inserePosicao :: Int->[Int]->Int->[Int]
inserePosicao e (cab:cauda) p
                    |p > 1 = [cab] ++ inserePosicao e cauda (p-1)
                    |p == 1 = [e] ++ inserePosicao e cauda (p-1)
                    |p == 0 = [cab] ++ inserePosicao e cauda p
inserePosicao e [] 0 = []

--troca um caractere de posição
trocaCaracter :: Char->Char->[Char]->[Char]
trocaCaracter a x (cab:cauda)
                              |cab == a = [x] ++ trocaCaracter a x cauda
                              |otherwise = [cab] ++ trocaCaracter a x cauda
trocaCaracter a x [] = []

inverte :: [Char]->[Char]
inverte (cab:cauda) = inverte cauda ++ [cab]
inverte [] = []

palindromo :: [Char]->Bool
palindromo x = palindromoAux x (inverte x)

palindromoAux :: [Char]->[Char]->Bool
palindromoAux (h1:t1) (h2:t2)
                            |h1 == h2 = palindromoAux t1 t2
                            |otherwise = False
palindromoAux [] [] = True

--verifica se um numero é primo
primo :: Int->Bool
primo x  = primoaux x 2

primoaux :: Int->Int->Bool
primoaux x y
            |x == 1 = False
            |x == y = True
            |mod x y == 0 = False
            |otherwise = primoaux x (y+1)

deltaSWhere :: Float->Float->Float->Float
deltaSWhere v v0 t = v0*t + (a*t*t)/2
                where
                  a = (v-v0)/t
deltaSLet :: Float->Float->Float->Float
deltaSLet v v0 t = let a = (v-v0)/t in v0*t + (a*t*t)/2

iniciais :: [Int]->Int->[Int]
iniciais (cab:cauda) y
                      |y >= 1 = [cab] ++ iniciais cauda (y-1)
                      |otherwise = []

inverteFinal :: [Int]->[Int]
inverteFinal (cab:cauda) = inverteFinal cauda ++ [cab]
inverteFinal [] = []

finais :: [Int]->Int->[Int]
finais x y = finaisAux (inverteFinal x) y

finaisAux :: [Int]->Int->[Int]
finaisAux (cab:cauda) y
                        |y >= 1 = [cab] ++ finaisAux cauda (y-1)
                        |otherwise = []
--sequencia de fibonacci
nEsimoFibo :: Int->Int
nEsimoFibo x = nEsimoFibo' x 0 1
    where
      nEsimoFibo' x a1 a2
        |x == 1 = a2
        |x > 1 = nEsimoFibo' (x-1) a2 (a1+a2)

-- soma de dois numeros naturais com e sem cauda
somaNat :: Int->Int->Int
somaNat x y
    |y == 0 = x
    |x == 0 = y
    |x > y = 1 + somaNat x (y-1)
    |x < y = 1 + somaNat (x-1) y

somaNatCauda :: Int->Int->Int
somaNatCauda x y
  |x == 0 = y
  |y == 0 = x
  |x > y = somaNatCauda (x + 1) (y - 1)
  |x < y = somaNatCauda (x - 1) (y + 1)
