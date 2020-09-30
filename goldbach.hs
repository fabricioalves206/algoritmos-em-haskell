primo :: Int->Bool
primo x  = primoaux x 2

primoaux :: Int->Int->Bool
primoaux x y
            |x == 1 = False
            |x == y = True
            |mod x y == 0 = False
            |otherwise = primoaux x (y+1)

listaprimo :: Int->[Int]
listaprimo x = listaprimoaux [1..x]

listaprimoaux :: [Int]->[Int]
listaprimoaux (cab:cauda) =if primo cab == True
                        then [cab] ++ listaprimoaux cauda
                        else listaprimoaux cauda
listaprimoaux [] = []

goldbach :: Int->[Int]
goldbach 2 = [0]
goldbach x = if mod x 2 == 0
            then goldbachaux (qsort (listaprimo x)) x
            else [0]

qsort :: [Int]->[Int]
qsort [] = []
qsort (cab:cauda) = qsort [ y | y <- cauda, y > cab]
                      ++ [cab]
                      ++ qsort [y | y <- cauda, y <=  cab]

goldbachaux :: [Int]->Int->[Int]
goldbachaux (cab:cauda) x =   if primo z == True
                              then [cab] ++ [z]
                              else goldbachaux cauda x
                                where
                                  z = x - cab
