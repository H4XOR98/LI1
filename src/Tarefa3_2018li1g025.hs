-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g025 where

import LI11819
import Tarefa0_2018li1g025
import Tarefa1_2018li1g025
import Data.List.Split
import Data.Char

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [ Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (1,1) D 2 1 0 , Jogador (5,1) D 1 0 2 , Jogador (6,5) E 0 2 2 , Jogador (1,5) C 3 0 0] [DisparoCanhao 0 (3,1) B , DisparoLaser 1 (5,2) D , DisparoCanhao 2 (6,4) E , DisparoChoque 3 2],
             Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (1,8) D 2 1 0 , Jogador (5,11) C 1 0 2 , Jogador (1,10) B 3 0 0] [],
             Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [] []]

-- * Funções principais da Tarefa 3.
-- ** Funções de compressão.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -- ^ 'Estado' a comprimir.
         -> String -- ^ String com 'Estado' comprimido.
comprime (Estado m j d) = comprimeTamanho m ++ comprimeMapa m ++ comprimeJogador j ++ comprimeDisparos d

-- *** Funções de compressão do 'Mapa'.

-- | Comprime o valor do tamanho de uma 'Matriz' numa String.
comprimeTamanho :: Mapa -- ^ 'Mapa' a retirar o tamanho.
                -> String -- ^ String com tamanho comprimido.
comprimeTamanho m | x == y = show x ++ ";"
                  | otherwise = show x ++ "," ++ show y ++ ";"
               where (x,y) = dimensaoMatriz m

-- | Comprime o 'Mapa' numa String.
comprimeMapa :: Mapa -- ^ 'Mapa' a comprimir.
             -> String -- ^ String com 'Mapa' comprimido.
comprimeMapa m | mapaVazio m = ";"
               | otherwise = comprimeMapaRepetidos (mapaToString (mapaToList m)) 1

-- | Junta as 'Peca's repetidas na String.
--
-- >>> comprimeMapaRepetidos "IIIIVVVD"
-- "4I3VD"
comprimeMapaRepetidos :: String -- ^ String a juntar.
                   -> Int -- ^ Número de vezes que está repetido.
                   -> String -- ^ String resultante.
comprimeMapaRepetidos "" _ = ";"
comprimeMapaRepetidos [h] a = show a ++ [h] ++ ";"
comprimeMapaRepetidos (h:t) a | h == head t = comprimeMapaRepetidos t (a+1)
                           | otherwise = if a /= 1 then show a ++ take 1 (h:t) ++ comprimeMapaRepetidos t 1 else take 1 (h:t) ++ comprimeMapaRepetidos t 1

-- | Verifica se o 'Mapa' é vazio, i.e ,se tirando as bordas, o 'Mapa' é composto apenas por 'Peca's 'Vazia's.  
mapaVazio :: Mapa -- ^ 'Mapa' a verificar.
          -> Bool -- ^ Resultado da verificação.
mapaVazio = all (==Vazia) . concat . tail . init . map (init . tail)

-- | Converte um 'Mapa' numa lista de 'Peca's sem as bordas.
mapaToList :: Mapa -- ^ 'Mapa' a converter.
           -> [Peca] -- ^ Lista resultante.
mapaToList = concat . tail . init . map (init . tail)

-- | Converte uma lista de 'Peca's para uma String.
mapaToString :: [Peca] -- ^ Lista a converter.
             -> String -- ^ String resultante.
mapaToString = concatMap showPeca

-- | Converte uma 'Peca' numa String.
showPeca :: Peca -- ^ 'Peca' a converter.
         -> String -- ^ String resultante.
showPeca Vazia = "V"
showPeca (Bloco Indestrutivel) = "I"
showPeca (Bloco Destrutivel) = "D"

-- *** Funções de compressão dos 'Jogador'es.

-- | Comprime uma lista de 'Jogador'es numa String.
comprimeJogador :: [Jogador] -- ^ Lista a comprimir.
                -> String -- ^ String resultante da compressão.
comprimeJogador [] = ";"
comprimeJogador (Jogador (x,y) dJ vJ lJ cJ : t) = "P" ++ show x ++ "," ++ show y ++ "," ++ show dJ ++ "," ++ show vJ ++ "," ++ show lJ ++ "," ++ show cJ ++ comprimeJogador t

-- *** Funções de compressão dos 'Disparo's.

-- | Comprime uma lista de 'Disparo's numa String.
comprimeDisparos :: [Disparo] -- ^ Lista a comprimir.
                 -> String -- ^ String resultante da compressão.
comprimeDisparos = concatMap comprimeDisparo

-- | Comprime um 'Disparo' numa String.
comprimeDisparo :: Disparo -- ^ 'Disparo' a comprimir.
                -> String -- ^ String resultante da compressão.
comprimeDisparo (DisparoCanhao jD (x,y) dD) = "TC" ++ show jD ++ "," ++ show x ++ "," ++ show y ++ "," ++ show dD
comprimeDisparo (DisparoLaser jD (x,y) dD) = "TL" ++ show jD ++ "," ++ show x ++ "," ++ show y ++ "," ++ show dD
comprimeDisparo (DisparoChoque jD tD) = "TS" ++ show jD ++ "," ++ show tD

-- ** Funções de descompressão.

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -- ^ String a descomprimir.
            -> Estado -- ^ 'Estado' resultante da descompressão.
descomprime s = Estado (descomprimeM s) (descomprimeJ s) (descomprimeD s)

-- *** Funções de descompressão do 'Mapa'.

-- | Descomprime uma String num 'Mapa'
descomprimeM :: String -- ^ String a descomprimir.
             -> Mapa -- ^ 'Mapa' resultante da descompressão.
descomprimeM s | (a !! 1) == "" =  descomprimeMapaTamanho (head a)
               | otherwise = descomprimeMapa (descomprimeString(a !! 1)) (descomprimeMapaTamanho (head a)) (1,1)
               where a = splitOn ";" s

-- | Descomprime uma String de 'Peca's num 'Mapa' 
descomprimeMapa :: String -- ^ String a descomprimir.
                -> Mapa -- ^ 'Mapa' no qual se adicionam as 'Peca's da String.
                -> Posicao -- ^ 'Posicao' a adicionar as 'Peca's.
                -> Mapa -- ^ 'Mapa' resultante da descompressão.
descomprimeMapa "" m _ = m
descomprimeMapa s m (a,b) | not (eBordaMatriz (a,b) m) = descomprimeMapa (tail s) (atualizaPosicaoMatriz (a,b) (showPecaString s) m) (nextPosMatriz (a, b) m)
                          | otherwise = descomprimeMapa s m (nextPosMatriz (a, b) m)

-- | Descomprime uma String com o tamanho do 'Mapa' num 'Mapa' vazio.
descomprimeMapaTamanho :: String -- ^ String a descomprimir.
                       -> Mapa -- ^ 'Mapa' vazio resultante da descompressão.
descomprimeMapaTamanho "" = []
descomprimeMapaTamanho s = mapaInicial $ mapaTamanho s

-- | Descomprime uma String com as 'Peca's repetidas juntas numa String com as 'Peca's separadas.
--
-- >>> descomprimeString "4I3VD"
-- "IIIIVVVD"
descomprimeString :: String -- ^ String a descomprimir.
                  -> String -- ^ String descomprimida.
descomprimeString "" = ""
descomprimeString s = replicate (repetidos s) (charc s) ++ descomprimeString (tail $ cauda s)

-- | Descomprime uma String com o tamanho do 'Mapa' numa 'Dimensao'.
mapaTamanho :: String -- ^ String a descomprimir.
            -> Dimensao -- ^ 'Dimensao' resultante.
mapaTamanho s = (read (head a)::Int , read (last a)::Int)
                      where a = splitOn "," s

-- | Devolve o número de 'Peca's repetidas na String.
--
-- >>> repetidos "4I3VD"
-- "4"
--
-- >>> descomprimeString "D"
-- "1"
repetidos :: String -- ^ String a verificar.
          -> Int -- ^ Número de 'Peca's repetidas.
repetidos s = if takeWhile isDigit s == "" then 1 else read $ takeWhile isDigit s

-- | Devolve o Char relacionado com o número de vezes que é repetido.
--
-- >>> charc "4I3VD"
-- 'I'
charc :: String -- ^ String a verificar.
      -> Char -- ^ Char resultante.
charc t = head $ cauda t

-- | Devolve a cauda de uma String quando não é dígito.
--
-- >>> cauda "4I3VD"
-- 'I3VD'
cauda :: String -- ^ String a verificar.
      -> String -- ^ String resultante.
cauda = dropWhile isDigit 

-- | Converte o primeiro elemento de uma String numa 'Peca'.
showPecaString :: String -- ^ String a verificar.
               -> Peca -- ^ 'Peca' resultante.
showPecaString s | head s == 'V' = Vazia
                 | head s == 'I' = Bloco Indestrutivel
                 | head s == 'D' = Bloco Destrutivel

-- *** Funções de descompressão dos 'Jogador'es.

-- | Descomprime uma String numa lista de 'Jogador'es.
descomprimeJ :: String -- ^ String a descomprimir.
             -> [Jogador] -- ^ Lista resultante da descompressão.
descomprimeJ s = descomprimeJogadores (splitOn ";" s !! 2)

-- | Descomprime uma String num 'Jogador'.
descomprimeJogador :: String -- ^ String a descomprimir.
                   -> Jogador -- ^ 'Jogador' resultante da descompressão.
descomprimeJogador s = Jogador (read (head a)::Int,read (a !! 1)::Int) (read (a !! 2)::Direcao) (read (a !! 3)::Int)  (read (a !! 4)::Int) (read (a !! 5)::Int)
                     where a = splitOn "," s

-- | Descomprime uma String de 'Jogador'es comprimidos numa lista de 'Jogador'es.
descomprimeJogadores :: String -- ^ String a descomprimir.
                     -> [Jogador] -- ^ Lista resultante da descompressão.
descomprimeJogadores s = map descomprimeJogador a
                     where a = tail $ splitOn "P" s

-- *** Funções de descompressão dos 'Disparo's.

-- | Descomprime uma String numa lista de 'Disparo's.
descomprimeD :: String -- ^ String a descomprimir. 
             -> [Disparo] -- ^ Lista resultante da descompressão.
descomprimeD s = descomprimeDisparos (a !! 3)
               where a = splitOn ";" s
      
-- | Descomprime uma String num 'Disparo'.         
descomprimeDisparo :: String  -- ^ String a descomprimir. 
                   -> Disparo -- ^ 'Disparo' resultante da descompressão.
descomprimeDisparo s | head (head a) == 'C' = DisparoCanhao (read (drop 1 (head a))::Int) (read (a !! 1) :: Int, read (a !! 2) :: Int) (read (a !! 3)::Direcao)
                     | head (head a) == 'L' = DisparoLaser (read (drop 1 (head a))::Int) (read (a !! 1) :: Int, read (a !! 2) :: Int) (read (a !! 3)::Direcao)
                     | head (head a) == 'S' = DisparoChoque (read (drop 1 (head a))::Int) (read (a !! 1) :: Int)
                     where a = splitOn "," s

-- | Descomprime uma String de 'Disparoes comprimidos numa lista de 'Disparo's.
descomprimeDisparos :: String  -- ^ String a descomprimir. 
                    -> [Disparo] -- ^ Lista resultante da descompressão.
descomprimeDisparos s = map descomprimeDisparo a
                     where a = tail $ splitOn "T" s