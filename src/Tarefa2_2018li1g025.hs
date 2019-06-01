-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g025 where

import LI11819
import Tarefa0_2018li1g025
import Tarefa1_2018li1g025



-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).


testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,Movimenta B,     Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (2,Movimenta E,     Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Movimenta D,     Estado mapa1 [Jogador (3,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Movimenta B,     Estado mapa1 [Jogador (3,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Movimenta C,     Estado mapa1 [Jogador (2,1) D 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (1,Dispara Laser,   Estado mapa1 [Jogador (2,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (2,Dispara Canhao,  Estado mapa1 [Jogador (2,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Movimenta C,     Estado mapa1 [Jogador (2,1) D 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Dispara Canhao,  Estado mapa1 [Jogador (1,1) E 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Dispara Canhao,  Estado mapa1 [Jogador (1,1) C 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Dispara Laser,   Estado mapa1 [Jogador (1,1) C 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Dispara Laser,   Estado mapa1 [Jogador (1,1) E 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),  
            (0,Dispara Canhao,  Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoCanhao 0 (1,1) E , DisparoCanhao 0 (1,1) C]),
            (0,Dispara Canhao,  Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoLaser 1 (5,2) D]),
            (1,Dispara Laser,   Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoCanhao 0 (2,1) B , DisparoLaser 1 (5,2) D]),
            (3,Dispara Laser,   Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoCanhao 0 (2,1) B , DisparoLaser 1 (5,2) D]),
            (3,Movimenta D,     Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoCanhao 0 (2,1) B , DisparoLaser 1 (5,2) D]),
            (2,Dispara Choque,  Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoCanhao 0 (2,1) B , DisparoLaser 1 (5,2) D]),
            (1,Dispara Choque,  Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (1,Movimenta D,     Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoChoque 1 5]),
            (1,Movimenta E,     Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoChoque 1 5]),
            (0,Movimenta B,     Estado mapa1 [Jogador (1,3) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoChoque 1 5]),
            (0,Movimenta E,     Estado mapa1 [Jogador (1,3) B 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] [DisparoChoque 1 5]),
            (3,Movimenta C,     Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 3 0 0] []),
            (2,Movimenta E,     Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 1 2 2,Jogador (5,1) C 3 0 0] []),
            (0,Dispara Laser,   Estado mapa1 [Jogador (1,1) B 2 1 0, Jogador (1,5) D 1 1 1,Jogador (6,5) E 1 2 2,Jogador (3,1) C 3 0 0] []),
            (0,Movimenta D,     Estado mapa2 [Jogador (1,1) D 2 1 0, Jogador (1,5) D 1 0 1,Jogador (6,5) E 0 2 2,Jogador (5,1) C 0 0 0] []),
            (0,Movimenta D,     Estado mapa3 [Jogador (1,1) D 2 1 0, Jogador (1,6) D 1 0 1,Jogador (7,5) E 0 2 2,Jogador (9,13) C 1 0 2] [])]
            where
                mapa1 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
                mapa2 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
                mapa3 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]



-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada n (Movimenta d) (Estado mE jE dE) = movimentaJogador n (Movimenta d) (Estado mE jE dE)
jogada n (Dispara Canhao) (Estado mE jE dE) = disparaCanhaoJogador n (Dispara Canhao) (Estado mE jE dE)
jogada n (Dispara Laser) (Estado mE jE dE) = disparaLaserJogador n (Dispara Laser) (Estado mE jE dE)
jogada n (Dispara Choque) (Estado mE jE dE) = disparaChoqueJogador n (Dispara Choque) (Estado mE jE dE)


                                            

-- | Verifica se um 'Jogador' se encontra vivo.
jogadorVivo :: Jogador -- ^ O 'Jogador' a verificar.
            -> Bool -- ^ O resultado da verificação.
jogadorVivo (Jogador _ _ n _ _) | n > 0 = True
                                | otherwise = False

-----------------------------------------------------------------

-- | Atualiza o 'Estado' de um determinado 'Jogador' após uma jogada de 'Movimenta' numa determinada 'Direcao'.
movimentaJogador :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                 -> Jogada -- ^ A 'Jogada' a efetuar.
                 -> Estado -- ^ O 'Estado' anterior.
                 -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
movimentaJogador n (Movimenta d) (Estado mE jE dE) = if jogadorVivo jogador then Estado mE (alteraLista n movimentaj jE) dE else Estado mE jE dE
                                                    where
                                                      jogador = (!!) jE n
                                                      movimentaj = verificaJogador 0 n (Movimenta d) (novaPosicaoJogador n (Movimenta d) mE jogador dE jE) jE mE






-- | Atualiza a lista de 'Jogador'es após o movimento de um dado 'Jogador'.
alteraLista :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
            -> Maybe Jogador -- ^ O 'Jogador' que efetua a jogada.
            -> [Jogador] -- ^ A lista de 'Jogador'es a atualizar.
            -> [Jogador] -- ^ a lista de 'Jogador'es alterada.
alteraLista _ Nothing x = x --O jogador não se pode movimentar, logo a lista e a mesma.
alteraLista n (Just x) listaJogadores = atualizaIndiceLista n x listaJogadores
 



-- | Verifica se um 'Jogador' se pode 'Movimenta'r, i.e. Não colide com outros 'Jogador'es ou com 'Parede's.

verificaJogador ::  Int -- ^ O acumulador para percorrer a lista de 'Jogador'es.
                -> Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                -> Jogada -- ^ A 'Jogada' a efetuar.
                -> Maybe Jogador  -- ^ O 'Jogador' que efetua a jogada.
                -> [Jogador] -- ^ A lista de 'Jogador'es. 
                -> Mapa -- ^ O 'Mapa'.
                -> Maybe Jogador  -- ^ O 'Jogador' após efetuar a jogada.
verificaJogador _ _ _ Nothing _ _  = Nothing -- O Jogador não se pode movimentar.
verificaJogador _ _ _ (Just j) [] _ = Just j 
verificaJogador acum indice jogada (Just (Jogador (lj,cj) dJj vJj lJj cJj)) (Jogador (l,c) dJ vJ lJ cJ:t) mapa | indice == acum = verificaJogador (acum+1) indice jogada (Just (Jogador (lj,cj) dJj vJj lJj cJj)) t mapa
                                                                                                               | otherwise = if colisaoJogadoresMapa (Jogador (l,c) dJ vJ lJ cJ) (Jogador (lj,cj) dJj vJj lJj cJj) 
                                                                                                                              then Nothing
                                                                                                                              else verificaJogador (acum+1) indice jogada (Just (Jogador (lj,cj) dJj vJj lJj cJj)) t mapa




-- | Calcula se há risco de colisão entre dois 'Jogador'es.
colisaoJogadoresMapa :: Jogador -- ^ O 'Jogador' a testar.
                     -> Jogador -- ^ O  'Jogador' a testar.
                     -> Bool -- ^ Resultado da verificação.
colisaoJogadoresMapa (Jogador (l,c) dJ vJ lJ cJ) (Jogador (lj,cj) dJj vJj lJj cJj) | jogadorVivo (Jogador (l,c) dJ vJ lJ cJ) && distanciaPosicoes (l, c) (lj, cj) < 2 = True -- se a distancia for menor que dois ocorre uma colisão
                                                                                   | otherwise = False



-- | Calcula a distância entre dois pontos.
distanciaPosicoes :: Posicao -- ^ A 'Posicao' de um 'Jogador'.
                  -> Posicao -- ^ A 'Posicao' de um 'Jogador'.
                  -> Float -- ^ Resultado do cálculo distância.
distanciaPosicoes (l1,c1) (l2,c2) = sqrt(fromIntegral((l1-l2)^2 + (c1-c2)^2))





-- | Nova 'Posicao' do 'Jogador' com base uma jogada numa 'Direcao'.
novaPosicaoJogador :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                   -> Jogada -- ^ A 'Jogada' a efetuar.
                   -> Mapa -- ^ O 'Mapa'.
                   -> Jogador -- ^ O 'Jogador' que efetua a jogada.
                   -> [Disparo] -- ^ A lista de 'Disparo's.
                   -> [Jogador] -- ^ A lista de 'Jogador'es.
                   ->  Maybe Jogador -- ^ O 'Jogador' após efetuar a jogada.
novaPosicaoJogador n (Movimenta d) mapa  (Jogador (l,c) dJ vJ lJ cJ) disparos jogadores | d /= dJ || afetadoPorChoque = Just (Jogador (l,c) d vJ lJ cJ)
                                                                                        | d == dJ && d == C && posValida (l-1,c) mapa && not afetadoPorChoque = Just (Jogador (l-1,c) dJ vJ lJ cJ)
                                                                                        | d == dJ && d == D && posValida (l,c+1) mapa && not afetadoPorChoque = Just (Jogador (l,c+1) dJ vJ lJ cJ)
                                                                                        | d == dJ && d == B && posValida (l+1,c) mapa && not afetadoPorChoque = Just (Jogador (l+1,c) dJ vJ lJ cJ)
                                                                                        | d == dJ && d == E && posValida (l,c-1) mapa && not afetadoPorChoque = Just (Jogador (l,c-1) dJ vJ lJ cJ)
                                                                                        | otherwise = Nothing
                                                                                      where
                                                                                        afetadoPorChoque = afetadoDisparoChoque n (Jogador (l,c) dJ vJ lJ cJ) disparos jogadores
                                                                 

-- | Verifica se uma 'Posicao' do tanque é válida.
--Inclui todas as pecas do tanque.
posValida :: Posicao -- ^ A 'Posicao' de um 'Jogador'.
          -> Mapa -- ^ O 'Mapa'.
          -> Bool -- ^ O resultado da verificação.
posValida (l,c) mapa = encontraPosicaoMatriz (l, c) mapa == Vazia && encontraPosicaoMatriz (l + 1, c) mapa == Vazia &&  encontraPosicaoMatriz (l, c + 1) mapa == Vazia &&  encontraPosicaoMatriz (l + 1, c + 1) mapa == Vazia


---------------------------------------------------------------------------

--DISPARO CANHÃO


-- | Atualiza o 'Estado' após um um 'Jogador' efetuar a jogada 'DisparoCanhao'.
disparaCanhaoJogador :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                     -> Jogada -- ^ A 'Jogada' a efetuar.
                     -> Estado -- ^ O 'Estado' anterior.
                     -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
disparaCanhaoJogador n (Dispara Canhao) (Estado mE jE dE) = if jogadorVivo jogador then Estado mE jE (disparo:dE) else Estado mE jE dE
                                                          where
                                                            jogador = (!!) jE n
                                                            direcao = getDirecaoJogador jogador
                                                            posicao = getPosicaoJogador jogador
                                                            disparo = posicaoDisparoCanhao n posicao direcao
                                                            
-- | Determina a 'Direcao' de um 'Jogador'.                                                                                                                       
getDirecaoJogador :: Jogador  -- ^ O 'Jogador' que efetua a jogada.
                  -> Direcao  -- ^ A 'Direcao' do 'Jogador'.
getDirecaoJogador  (Jogador (l,c) dJ vJ lJ cJ) = dJ



-- | Determina da 'Posicao' de um 'Jogador'. 
getPosicaoJogador :: Jogador  -- ^ O 'Jogador' que efetua a jogada.
                  -> Posicao -- ^ A 'Posicao' do 'Jogador'.
getPosicaoJogador (Jogador (l,c) dJ vJ lJ cJ) = (l,c)



-- | Determina 'DisparoCanhao'.
posicaoDisparoCanhao :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                     -> Posicao -- ^ A 'Posicao' do 'Jogador'.
                     -> Direcao  -- ^ A 'Direcao' do 'Jogador'.
                     -> Disparo -- ^ O 'Disparo' resultante.
posicaoDisparoCanhao n (l,c) d | d == C = DisparoCanhao n (l-1,c) d -- O jogador está voltado para Cima logo o disparo é de uma só coluna
                               | d == E = DisparoCanhao n (l,c-1) d
                               | d == B = DisparoCanhao n (l+1,c) d
                               | d == D = DisparoCanhao n (l,c+1) d




--DISPARO LASER

-- | Atualiza o 'Estado' após um um 'Jogador' efetuar a jogada 'DisparoLaser'.
disparaLaserJogador :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                    -> Jogada -- ^ A 'Jogada' a efetuar.
                    -> Estado -- ^ O 'Estado' anterior.
                    -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
disparaLaserJogador n (Dispara Laser) (Estado mE jE dE) = if numeroDisparosLaser jogador && jogadorVivo jogador
                                                          then Estado mE listaJogadorAtualizada (disparo:dE)
                                                          else Estado mE jE dE
                                                                where
                                                                  jogador = (!!) jE n
                                                                  direcao = getDirecaoJogador jogador
                                                                  posicao = getPosicaoJogador jogador
                                                                  disparo = posicaoDisparoLaser n posicao direcao
                                                                  listaJogadorAtualizada = atualizaIndiceLista n (decrementoLaserJogador jogador) jE


-- | Verifica se um 'Jogador' possui 'DisparoLaser'.
numeroDisparosLaser :: Jogador -- ^ O 'Jogador' que efetua a jogada.
                    -> Bool -- ^ O Resultado da verificação.
numeroDisparosLaser (Jogador (l,c) dJ vJ lJ cJ) | lJ > 0 = True
                                                | otherwise = False


-- | Decrementa o numero de 'DisparoLaser' que o 'Jogador' pode efetuar.
decrementoLaserJogador :: Jogador -- ^ O 'Jogador' a atualizar.
                       -> Jogador -- ^ O 'Jogador' atualizado.
decrementoLaserJogador (Jogador (l,c) dJ vJ lJ cJ) = Jogador (l, c) dJ vJ (lJ - 1) cJ


-- | Determina 'DisparoLaser'.
posicaoDisparoLaser :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                    -> Posicao -- ^ A 'Posicao' do 'Jogador'.
                    -> Direcao  -- ^ A 'Direcao' do 'Jogador'.
                    -> Disparo -- ^ O 'Disparo' resultante.
posicaoDisparoLaser n (l,c) d | d == C = DisparoLaser n (l-1,c) d -- O jogador está voltado para cima logo o disparo é de uma só coluna
                              | d == E = DisparoLaser n (l,c-1) d
                              | d == B = DisparoLaser n (l+1,c) d
                              | d == D = DisparoLaser n (l,c+1) d





--DISPARO CHOQUE
-- | Atualiza o 'Estado' após um um 'Jogador' efetuar a jogada 'DisparoChoque'.
disparaChoqueJogador :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                     -> Jogada -- ^ A 'Jogada' a efetuar.
                     -> Estado -- ^ O 'Estado' anterior.
                     -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
disparaChoqueJogador n (Dispara Choque) (Estado mE jE dE) = if numeroDisparosChoque jogador && jogadorVivo jogador
                                                            then Estado mE listaJogadorAtualizada (disparo:dE)
                                                            else Estado mE jE dE
                                                                where
                                                                  jogador = (!!) jE n
                                                                  disparo = posicaoDisparoChoque n 5
                                                                  listaJogadorAtualizada = atualizaIndiceLista n (decrementoChoqueJogador jogador) jE


-- | Verifica se um 'Jogador' possui 'DisparoChoque'.
numeroDisparosChoque:: Jogador -- ^ O 'Jogador' que efetua a jogada.
                    -> Bool -- ^ O resultado da verificação.
numeroDisparosChoque (Jogador (l,c) dJ vJ lJ cJ) | cJ > 0 = True
                                                 | otherwise = False


-- | Decrementa o numero de 'DisparoChoque' que o 'Jogador' pode efetuar.
decrementoChoqueJogador :: Jogador -- ^ O 'Jogador' a atualizar.
                        -> Jogador -- ^ O 'Jogador' atualizado.
decrementoChoqueJogador (Jogador (l,c) dJ vJ lJ cJ) = Jogador (l, c) dJ vJ lJ (cJ - 1)


-- | Determina 'DisparoChoque'.
posicaoDisparoChoque :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                     -> Ticks -- ^ O número de instantes de tempo do jogo.
                     -> Disparo -- ^ O 'Disparo' resultante.
posicaoDisparoChoque = DisparoChoque 

-----------------------------------------------------------------------------


-- | Verifica se um 'Jogador' está afetado por um 'Choque'.
riscoDeChoque :: Jogador -- ^ O 'Jogador' que efetua o 'Choque'.
              -> Jogador -- ^ O 'Jogador' a verificar.
              -> Bool -- ^ O resultado da verificação.
riscoDeChoque (Jogador (l1,c1) dJ1 vJ1 lJ1 cJ1) (Jogador (l2,c2) dJ2 vJ2 lJ2 cJ2) | (dif1<=3) && (dif2<=3) = True
                                                                                  | otherwise = False
                                                                                  where
                                                                                    dif1 = abs (l1-l2)
                                                                                    dif2 = abs (c1-c2)



-- | Verifica se algum 'Jogador' está afetado por um 'DisparoChoque'.
-- Percorre a lista disparo para perceber se o jogador é ou não afetado por um disparo Choque.
afetadoDisparoChoque :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.  
                     -> Jogador -- ^ O 'Jogador' que efetua o 'Choque'.
                     -> [Disparo] -- ^ A lista de 'Disparo's.
                     -> [Jogador] -- ^ A lista de 'Jogador'es.
                     -> Bool -- ^ O resultado da verificação.
afetadoDisparoChoque _ _ [] _ = False
afetadoDisparoChoque nJ jogador (DisparoChoque nJD x : t) jogadores | nJ == nJD = afetadoDisparoChoque nJ jogador t jogadores 
                                                                    | riscoDeChoque jogador ((!!) jogadores nJD) = True
                                                                    | otherwise =  afetadoDisparoChoque nJ jogador t jogadores
afetadoDisparoChoque nJ jogador (h:t) jogadores = afetadoDisparoChoque nJ jogador t jogadores