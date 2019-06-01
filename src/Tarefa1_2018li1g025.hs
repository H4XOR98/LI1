-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g025 where

import Tarefa0_2018li1g025
import LI11819

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move D],
           [Move D,Move B],
           [Move D,Move B,Roda],
           [Move D,Move B,Roda,MudaTetromino],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B,Move E],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B,Move E,MudaParede],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B,Move E,MudaParede,Move E],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B,Move E,MudaParede,Move E,Move B],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B,Move E,MudaParede,Move E,Move B,MudaTetromino],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B,Move E,MudaParede,Move E,Move B,MudaTetromino,MudaTetromino],
           [Move D,Move B,Roda,MudaTetromino,Move B,Desenha,Move D,Move B,Move E,MudaParede,Move E,Move B,MudaTetromino,MudaTetromino,MudaTetromino,Desenha],
           [Desenha,Move D,Move D,Roda,Desenha,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Roda,Desenha,Move E,Move E,Roda,Desenha,Move E,Move E,Roda,Desenha,MudaTetromino,Move B,Move B,Desenha,Move B,Move B,Roda,Desenha,Move D,Move D,Roda,Desenha,Move D,Move D,Roda,Desenha,Move D,Move D,Roda,Desenha,MudaTetromino,Move C,Move C,Desenha,Move D,Move D,Roda,Desenha,Move C,Move C,Roda,Desenha,Move C,Move C,Roda,Desenha,Move D,Move D,Roda,Desenha,Move D,Move D,MudaTetromino,Desenha,Move B,Move B,Move B,Roda,Desenha,Move B,Roda,Desenha,Move B,Roda,Move B,Roda,Desenha,Move D,Move D,MudaTetromino,Desenha,Move C,Move C,Roda,Desenha,Move C,Move C,Roda,Desenha,Move C,Move C,Roda,Desenha,Move C,Move C,Roda,Desenha,Move E,Move E,MudaTetromino,Move E,Move E,Desenha,Move E,Move E,Roda,Move E,Desenha,Move E,Move E,Roda,Roda,Move E,Desenha,Move E,Move E,Roda,Roda,Desenha,Move E,Move E,Roda,Desenha,Move E,Move E,Roda,Desenha,Move E,Roda,Desenha,Roda,Desenha,Roda,Desenha,Move B,Move B,MudaTetromino,Move B,Move B,Desenha,Roda,Desenha,Move B,Move B,Move B,Roda,Desenha,Move B,Move B,Move B,Roda,Desenha,Move D,Move D,Roda,Desenha,Move D,Move D,Roda,Desenha,Move D,Move D,MudaTetromino,Move B,Desenha,MudaParede,Move D,Move D,Desenha,Move C,Move C,Move C,Roda,Desenha,Move C,Move C,Roda,Desenha,Move E,Move E,Roda,Desenha,Move C,Move C,Move C,Roda,Desenha,Move E,Move E,MudaTetromino,Desenha,Move B,Move B,Move B,Roda,Desenha,Move E,Move E,Move E,Roda,Desenha,Move C,Move C,Move C,Move C,Roda,Desenha,Move C,Move C,Move C,Roda,Desenha,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,MudaTetromino,Move B,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Roda,Desenha,Move D,Move D,Move B,Move B,Move B,Roda,Desenha,Move B,Move B,Move B,Roda,Desenha,Move B,Move B,Move E,MudaTetromino,Move E,Move E,Move E,Desenha,Move D,Roda,Desenha,Move D,Roda,Desenha,Move D,Roda,Desenha,Move D,Roda,Desenha,Move B,MudaTetromino,Desenha,Move E,Move E,Roda,Desenha,Move E,Move E,Roda,Desenha,Move E,Move E,Roda,Desenha,Move E,Roda,Desenha,Move E,Roda,Desenha,Move E,Roda,Desenha,MudaTetromino,Move E,Move E,Roda,Desenha,Move E,Roda,Desenha,Move E,Move E,Roda,Desenha,Move E,Roda,Desenha,Move E,Roda,Desenha,MudaTetromino,Move C,Move C,Move C,Desenha,Move D,Roda,Desenha,Move D,Roda,Desenha,Move D,Roda,Desenha,Move D,Roda,Desenha,Move D,Roda,Desenha,MudaTetromino,Move D,Move D,Desenha,Move D,Move D,Move D,Move C,Move C,MudaTetromino,MudaParede,Desenha,Move E,Move E,Move B,Move B,Desenha,Move B,Move D,Desenha,MudaParede,Move D,Desenha]]
-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao  (Move orientacao) (Editor pC dT tT tP mE) = Editor (somaVetores pC (direcaoParaVetor orientacao)) dT tT tP mE
instrucao Roda (Editor pC dT tT tP mE) = Editor pC (nextOrientation dT) tT tP mE
instrucao MudaTetromino (Editor pC dT tT tP mE) = Editor pC dT (nextTetromino tT) tP mE
instrucao MudaParede (Editor pC dT tT tP mE) = Editor pC dT tT (nextWall tP) mE
instrucao Desenha (Editor pC dT tT tP mE) = Editor pC dT tT tP (desenha (Editor pC dT tT tP mE))

-- | Devolve a próxima 'Direcao' num 'Editor'.
nextOrientation :: Direcao -- ^ A 'Direcao' atual.
                -> Direcao -- ^ A próxima 'Direcao'.
nextOrientation C = D
nextOrientation D = B
nextOrientation B = E
nextOrientation E = C

-- | Devolve o próximo 'Tetromino' num 'Editor'.
nextTetromino :: Tetromino -- ^ O 'Tetromino' atual.
              -> Tetromino -- ^ O próximo 'Tetromino'.
nextTetromino I = J
nextTetromino J = L
nextTetromino L = O
nextTetromino O = S
nextTetromino S = T
nextTetromino T = Z
nextTetromino Z = I

-- | Devolve a próxima 'Parede' num 'Editor'.
nextWall :: Parede -- ^ A 'Parede' atual.
         -> Parede -- ^ A 'Parede' atual.
nextWall Indestrutivel = Destrutivel
nextWall Destrutivel = Indestrutivel

-- | Adiciona uma 'Posicao' a uma lista de 'Posicao'.
adicionaPosicao :: Posicao -- ^ 'Posicao' a adicionar.
                -> [Posicao] -- ^ Lista a alterar.
                -> [Posicao] -- ^ Lista alterada.
adicionaPosicao (x, y) = map (somaVetores (x, y))

-- | Adiciona a um lista de 'Posicao' as Posições em que a 'Matriz' de Bool é True.
procura :: Matriz Bool -- ^ A 'Matriz' a verificar.
        -> Posicao -- ^ 'Posicao' a verificar.
        -> [Posicao] -- ^ Lista de 'Posicao' em que a 'Matriz' é True.
procura m (x,y) | (x, y) < (a-1,b-1) && encontraPosicaoMatriz (x,y) m = (x,y):procura m (nextPosMatriz (x,y) m) 
                | (x, y) < (a-1,b-1) = procura m (nextPosMatriz(x,y) m)
                | (x, y) == (a-1,b-1) && encontraPosicaoMatriz (x,y) m = [(x,y)]
                | otherwise = []
                where (a,b) = dimensaoMatriz m

-- | Devolve a 'Posicao' seguinte numa 'Matriz'.
nextPosMatriz :: Posicao -- ^ 'Posicao' atual na 'Matriz'.
              -> Matriz a -- ^ 'Matriz' a percorrer.
              -> Posicao -- ^ 'Posicao' seguinta na 'Matriz'.
nextPosMatriz (x,y) m | x <= a-1 && y < b-1 = (x,y+1)
                        | x < a-1 && y >= b-1 = (x+1,0)
                      where (a,b) = dimensaoMatriz m

-- | Roda uma 'Matriz' segundo uma certa 'Direcao'.
rodaMatrizDirecao :: Direcao -- ^ 'Direcao' para a 'Matriz' rodar.
                  -> Matriz a -- ^ 'Matriz' a rodar.
                  -> Matriz a -- ^ 'Matriz' a rodada.
rodaMatrizDirecao C m = m
rodaMatrizDirecao D m = rodaMatriz m
rodaMatrizDirecao B m = rodaMatriz $ rodaMatrizDirecao D m
rodaMatrizDirecao E m = rodaMatriz $ rodaMatrizDirecao B m

-- | Escreve um 'Peca' no 'Mapa' segundo as definições do 'Editor'.
desenha :: Editor -- ^ 'Editor' com as definições para desenhar.
        -> Mapa -- ^ 'Mapa' atualizado com a 'Peca' desenhada.
desenha (Editor (x,y) dT tT tP mE) = foldl (\ m h -> atualizaPosicaoMatriz h (Bloco tP) m) mE listpos
                                         where 
                                          listpos = adicionaPosicao (x,y) (procura(rodaMatrizDirecao dT (tetrominoParaMatriz tT))(0,0))

-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes = flip $ foldl $ flip instrucao


-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (l,c) = alteraCabecaMapa mapa
                  where
                    mapa = criaMatriz (l,c) Vazia --Cria um mapa com todos os elementos em Vazia (elementos do tipo Peca).


-- | Modifica a cabeca do 'Mapa',i.e. a primeira linha.
alteraCabecaMapa :: Mapa -- ^ O 'Mapa' a modificar.
                 -> Mapa -- ^ O 'Mapa' a modificar.
alteraCabecaMapa [] = []
alteraCabecaMapa (h:t) = paredeLinha h : alteraRMapa t


-- | Modifica o restante 'Mapa'. 
alteraRMapa :: Mapa -- ^ O 'Mapa' a modificar.
            -> Mapa -- ^ O 'Mapa' a modificar.
alteraRMapa [] = []
alteraRMapa [x] = [paredeLinha x]
alteraRMapa (h:t) = paredeColuna h : alteraRMapa t


-- | Modifica uma lista de 'Peca's de 'Peca' para 'Bloco' 'Indestrutivel'. 
-- Utilizado na primeira e última linha do mapa. 
paredeLinha :: [Peca] -- ^ As 'Peca's a modificar.
            -> [Peca] -- ^ As 'Peca's resultantes.
paredeLinha = map (\ _ -> Bloco Indestrutivel)


-- | Modifica o primeiro e o último elemento de uma lista de 'Peca's de 'Peca' para 'Bloco Indestrutível'. 
-- Utilizado em todas as restantes linhas. 
paredeColuna :: [Peca] -- ^ As 'Peca's a modificar.
             -> [Peca] -- ^ As 'Peca's resultantes.
paredeColuna [] =[]
paredeColuna [_] = [Bloco Indestrutivel]
paredeColuna l = Bloco Indestrutivel : init (tail l) ++ [Bloco Indestrutivel]

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial ins = Editor (posicaoInicial ins) C I Indestrutivel (mapaInicial $ dimensaoInicial ins)


-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = mapaEditor (instrucoes is (editorInicial is))