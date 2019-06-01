-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g025 where

import LI11819

-- * Funções não-recursivas.
-- | Um 'Vetor' é uma 'Posicao' em relação à origem.


type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>


-- ** Funções sobre vetores
-- *** Funções gerais sobre 'Vetor'es.



-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -- ^ O 'Vetor' a somar.
            -> Vetor -- ^ O 'Vetor' a somar.
            -> Vetor -- ^ O 'Vetor' resultante da soma.
somaVetores (l1,c1) (l2,c2) = ( l1+l2 , c1+c2 )


-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -- ^ O 'Vetor' a subtrair.
               -> Vetor -- ^ O 'Vetor' a subtrair.
               -> Vetor -- ^ O 'Vetor' resultante da subtração.
subtraiVetores (l1,c1) (l2,c2) = ( l1-l2, c1-c2 ) 


-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -- ^ O Escalar a multiplicar.
                -> Vetor -- ^ O 'Vetor' a multiplicar.
                -> Vetor -- ^ O 'Vetor' resultante da multiplicação.
multiplicaVetor e (l,c) =  ( e*l , e*c )


-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -- ^ O 'Vetor' a rodar.
          -> Vetor -- ^ O 'Vetor' resultante da rotação. 
rodaVetor (l,c) = (c,-l)


-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -- ^ O 'Vetor' a espelhar.
              -> Vetor -- ^ O 'Vetor' espelhado na horizontal.
inverteVetorH (l,c) = (l,-c)


-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -- ^ O 'Vetor' a espelhar.
              -> Vetor -- ^ O 'Vetor' espelhado na vertical. 
inverteVetorV (l,c) = (-l,c)


-- *** Funções do trabalho sobre 'Vetor'es.



-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -- ^ A 'Direcao' a usar na criação do 'Vetor'.
                 -> Vetor -- ^ O 'Vetor' resultante com a 'Dimensao' dada.
direcaoParaVetor dir | dir == C = (-1,0)
                     | dir == D = (0,1)
                     | dir == B = (1,0)
                     | dir == E = (0,-1)


-- ** Funções sobre listas
-- *** Funções gerais sobre listas.


-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -- ^ O Índice de uma lista a verificar.
                   -> [a] -- ^ A Lista a verificar.
                   -> Bool -- ^ O resultado da verificação.
eIndiceListaValido i l = i >= 0 && length l > i




-- ** Funções sobre matrizes.
-- *** Funções gerais sobre matrizes.
-- | Uma matriz é um conjunto de elementos a duas dimensões.
-- Em notação matemática, é geralmente representada por:
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]


-- | Calcula a dimensão de uma matriz.
dimensaoMatriz :: Matriz a -- ^ A 'Matriz' a aplicar.
               -> Dimensao -- ^ A 'Dimensao' resultante.
dimensaoMatriz []  = (0,0)
dimensaoMatriz l | null l ||any null l = (0,0)
                 |  otherwise =  (length l,length $ head l)



-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -- ^ A 'Posicao' a verificar.
                     -> Matriz a -- ^ A 'Matriz' a ser verficada.
                     -> Bool -- ^ O resultado da verificação.
ePosicaoMatrizValida (l,c) lista = l >= 0 && c >= 0 && (l <= x-1) && (c <= y-1) 
                            where
                            (x,y) = dimensaoMatriz lista --Calcula a dimensao da matriz



-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -- ^ A 'Posicao' a verificar.
             -> Matriz a -- ^ A 'Matriz' a ser verficada.
             -> Bool -- ^ O resultado da verificação.
eBordaMatriz (l,c) lista = (l == 0 || l == x-1) || (c == 0 || c == y-1) 
                         where
                         (x,y) = dimensaoMatriz lista 





-- *** Funções do trabalho sobre matrizes.



-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -- ^ O 'Tetronomio' a converter.
                    -> Matriz Bool -- ^ A 'Matriz' resultante.
tetrominoParaMatriz t | t == I = [ [False,True,False,False] , [False,True,False,False] , [False,True,False,False] , [False,True,False,False]]
                      | t == J = [ [False,True,False] , [False,True,False] , [True,True,False] ]
                      | t == L = [ [False,True,False] , [False,True,False] , [False,True,True] ]
                      | t == O = [ [True,True] , [True,True] ]
                      | t == S = [ [False,True,True] , [True,True,False] , [False,False,False] ]
                      | t == T = [ [False,False,False] , [True,True,True] , [False,True,False] ]
                      | t == Z = [ [True,True,False] , [False,True,True] , [False,False,False] ]



-- * Funções recursivas.
-- ** Funções sobre listas.
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.



-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -- ^ O índice do elemento a devolver.
                    -> [a] -- ^ A lista a verificar.
                    -> a -- ^ O elemento resultante.
encontraIndiceLista = flip (!!)

-- | Modifica um elemento num dado índice.
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -- ^ O índice do elemento a modificar.
                    -> a  -- ^ O elemento resultante.
                    -> [a] -- ^ A lista a modificar.
                    -> [a] -- ^ A lista modificada.
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista i elemento (h:t) | i == 0 = elemento:t
                                 | otherwise = h : atualizaIndiceLista (i - 1) elemento t    



-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>

rodaMatriz :: Matriz a -- ^ A 'Matriz' a rodar.
           -> Matriz a -- ^ A 'Matriz' resultante.
rodaMatriz [] = [] 
rodaMatriz ([]:_) = []
rodaMatriz matriz = reverse (map head matriz) : rodaMatriz (map tail matriz)



-- | Inverte uma 'Matriz' na horizontal.

-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>

inverteMatrizH :: Matriz a -- ^ A 'Matriz' a inverter.
               -> Matriz a -- ^ A 'Matriz' resultante.
inverteMatrizH = map reverse

-- | Inverte uma 'Matriz' na vertical.

-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>


inverteMatrizV :: Matriz a -- ^ A 'Matriz' a inverter.
               -> Matriz a -- ^ A 'Matriz' resultante.
inverteMatrizV = reverse

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -- ^ A 'Dimensao' a aplicar.
           -> a -- ^ O elemento a utilizar.
           -> Matriz a -- ^ A 'Matriz' resultante.
criaMatriz (l,c) elemento | l > 0 = criaLinhasMat c elemento : criaMatriz (l - 1, c) elemento
                          | otherwise = []
                      where
                        criaLinhasMat c elemento | c > 0 = elemento : criaLinhasMat (c - 1) elemento
                                                  | otherwise = []




-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (l,c) (h:t) | l == 0 = encontraPosicaoLinha c h
                                  | otherwise = encontraPosicaoMatriz (l - 1, c) t
                                  where
                                    encontraPosicaoLinha c (h:t) | c == 0 = h
                                                                 | otherwise = encontraPosicaoLinha (c-1) t




-- | Modifica um elemento numa dada 'Posicao'.
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.


atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) elemento matriz | not (ePosicaoMatrizValida (l, c) matriz) = matriz
                                            | otherwise = percorreMatriz (l,c) elemento matriz

-- | Percorre a 'Matriz' até à linha pretendida e modifica um elemento. 
percorreMatriz :: Posicao -> a -> Matriz a -> Matriz a
percorreMatriz (_,_) _ [] = []
percorreMatriz (l,c) elemento (h:t) | l == 0 = percorreLista (l, c) elemento h : t
                                    | otherwise = h : percorreMatriz (l - 1, c) elemento t




-- | Percorre uma linha de uma matriz até à coluna pretendida.
percorreLista :: Posicao -> a -> [a] -> [a]
percorreLista _ _ [] = []
percorreLista (l,c) elemento (h:t) | c == 0 = substituiElem h elemento : t
                               | otherwise = h : percorreLista (l, c - 1) elemento t

-- | Substitui um elemento.
substituiElem :: a -> a -> a
substituiElem   _ elem = elem