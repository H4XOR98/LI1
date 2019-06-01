-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g025 where

import LI11819
import Tarefa0_2018li1g025

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.



testesT4 :: [Estado]
testesT4 = [
            Estado mapa1 [Jogador (1,1) D 3 3 3,Jogador (1,6) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (6,6) C 0 3 1] [DisparoLaser 0 (1,2) D,DisparoLaser 1 (1,5) E,DisparoLaser 2 (6,2) D],
            Estado mapa1 [Jogador (1,1) B 3 3 3,Jogador (1,6) B 3 3 3,Jogador (6,1) C 3 3 3,Jogador (6,6) E 0 3 1] [DisparoLaser 0 (2,1) B,DisparoLaser 1 (2,6) B,DisparoLaser 2 (5,1) C],
            Estado mapa2 [Jogador (1,1) D 3 3 3,Jogador (1,7) E 3 3 3,Jogador (7,1) D 3 3 3,Jogador (7,7) E 3 3 1] [DisparoLaser 0 (1,2) D,DisparoLaser 1 (1,6) E,DisparoLaser 2 (7,2) D,DisparoLaser 3 (7,6) E],
            Estado mapa1 [Jogador (2,1) D 3 3 3,Jogador (1,6) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (6,6) C 0 3 1] [DisparoLaser 0 (2,2) D,DisparoLaser 1 (1,5) E],
            Estado mapa1 [Jogador (2,1) B 3 3 3,Jogador (1,6) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (6,6) C 0 3 1] [DisparoLaser 0 (3,1) B,DisparoLaser 2 (5,1) C],
            Estado mapa1 [Jogador (1,1) D 3 3 3,Jogador (1,6) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (6,6) C 1 3 1] [DisparoCanhao 0 (1,2) D,DisparoLaser 1 (1,5) E,DisparoCanhao 3 (4,6) C,DisparoLaser 2 (6,2) D,DisparoCanhao 3 (5,6) C],
            Estado mapa2 [Jogador (2,1) B 3 3 3,Jogador (1,7) E 3 3 3,Jogador (7,1) D 3 3 3,Jogador (7,7) C 0 3 1] [DisparoCanhao 0 (2,2) D,DisparoLaser 1 (1,6) E,DisparoCanhao 3 (5,7) C,DisparoLaser 2 (7,2) D,DisparoCanhao 3 (6,7) C],
            Estado mapa1 [Jogador (1,1) B 3 3 3,Jogador (1,6) B 3 3 3,Jogador (6,1) C 3 3 3,Jogador (6,6) C 3 3 1] [DisparoCanhao 0 (2,1) B,DisparoLaser 2 (5,1) C,DisparoLaser 1 (2,6) B,DisparoCanhao 3 (5,6) C],
            Estado mapa2 [Jogador (2,1) B 3 3 3,Jogador (1,7) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (7,7) C 0 3 1] [DisparoLaser 2 (6,2) D],
            Estado mapa3 [Jogador (7,1) D 3 3 3,Jogador (1,3) B 3 3 3,Jogador (1,6) B 3 3 3,Jogador (7,7) B 0 0 0] [DisparoLaser 0 (7,2) D,DisparoLaser 1 (2,3) B,DisparoLaser 2 (2,6) B],
            Estado mapa3 [Jogador (2,1) D 3 3 3,Jogador (2,7) E 0 3 3,Jogador (1,4) B 3 3 3,Jogador (7,7) B 0 0 0] [DisparoLaser 0 (2,2) D,DisparoLaser 2 (2,4) B],
            Estado mapa3 [Jogador (7,1) D 0 3 3,Jogador (1,3) B 0 3 3,Jogador (1,5) B 3 3 3,Jogador (7,7) B 0 0 0] [DisparoLaser 2 (2,5) B],
            Estado mapa3 [Jogador (7,1) D 0 3 3,Jogador (1,3) B 0 3 3,Jogador (1,4) B 3 3 3,Jogador (7,7) B 0 0 0] [DisparoLaser 2 (2,4) B],
            Estado mapa4 [Jogador (7,1) D 0 3 3,Jogador (1,3) B 0 3 3,Jogador (1,4) B 3 3 3,Jogador (7,7) B 0 0 0] [DisparoLaser 2 (2,4) B],
            Estado mapa1 [Jogador (1,1) D 3 3 3,Jogador (1,6) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (6,6) E 1 3 1] [DisparoCanhao 0 (1,2) D,DisparoCanhao 3 (6,5) E],
            Estado mapa1 [Jogador (1,1) B 3 3 3,Jogador (1,6) C 3 3 3,Jogador (6,1) B 3 3 3,Jogador (6,6) C 1 3 1] [DisparoCanhao 0 (2,1) B,DisparoCanhao 3 (5,6) C],
            Estado mapa1 [Jogador (1,1) B 3 3 3,Jogador (1,6) C 3 3 3,Jogador (6,1) B 3 3 3,Jogador (6,6) C 1 3 1] [DisparoCanhao 2 (2,1) C,DisparoCanhao 0 (5,1) B,DisparoCanhao 1 (1,2) E,DisparoCanhao 2 (6,5) D],
            Estado mapa1 [Jogador (1,1) B 3 3 3,Jogador (1,6) C 3 3 3,Jogador (6,1) B 3 3 3,Jogador (6,6) C 1 3 1] [DisparoCanhao 0 (1,4) D,DisparoCanhao 1 (1,4) E,DisparoCanhao 0 (4,1) B,DisparoCanhao 2 (3,1) C],
            Estado mapa1 [Jogador (1,1) B 3 3 3,Jogador (1,6) C 3 3 3,Jogador (6,1) B 3 3 3,Jogador (6,6) C 1 3 1] [DisparoCanhao 2 (6,4) D,DisparoCanhao 3 (6,3) E, DisparoCanhao 1 (4,6) B, DisparoCanhao 3 (4,6) C],
            Estado mapa2 [Jogador (1,1) D 3 3 3,Jogador (1,7) D 3 3 3,Jogador (6,1) D 3 3 3,Jogador (7,7) E 3 3 1] [DisparoCanhao 0 (1,3) D,DisparoCanhao 2 (6,3) D,DisparoCanhao 1 (1,8) D],
            Estado mapa3 [Jogador (6,1) D 0 3 3,Jogador (1,2) B 0 3 3,Jogador (1,4) B 3 3 3,Jogador (1,6) B 3 3 3] [DisparoCanhao 1 (2,2) B,DisparoCanhao 2 (2,4) B,DisparoCanhao 3 (2,6) B],
            Estado mapa4 [Jogador (7,1) D 0 3 3,Jogador (1,3) B 0 3 3,Jogador (1,4) B 3 3 3,Jogador (7,7) B 0 0 0] [DisparoCanhao 2 (2,4) B],
            Estado mapa4 [Jogador (7,1) D 0 3 3,Jogador (1,3) B 0 3 3,Jogador (1,3) B 3 3 3,Jogador (7,7) B 0 0 0] [DisparoCanhao 2 (2,3) B],
            Estado mapa1 [Jogador (1,1) D 3 3 3,Jogador (1,6) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (6,6) C 2 3 1] [DisparoChoque 0 3,DisparoChoque 1 0,DisparoChoque 2 1,DisparoCanhao 0 (1,2) D,DisparoChoque 3 5,DisparoCanhao 1 (1,5) E],
            Estado mapa1 [Jogador (1,1) D 3 3 3,Jogador (1,6) E 3 3 3,Jogador (6,1) D 3 3 3,Jogador (6,6) C 2 3 1] [DisparoChoque 0 3,DisparoChoque 1 0,DisparoChoque 2 1,DisparoChoque 3 5,DisparoCanhao 0 (1,2) D,DisparoLaser 1 (1,5) E,DisparoCanhao 3 (4,6) C,DisparoLaser 2 (6,2) D,DisparoCanhao 3 (5,6) C]
           ]
          where
            mapa1 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
            mapa2 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
            mapa3 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
            mapa4 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]



-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.

tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers = colisaoDisparoLaserComMapa.colisaoDisparosLaser.colisaoDisparoLaserComJogador


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes = atualizaEstadoCanhao.colisaoDisparoCanhaoComMapa.colisaoDisparoCanhao.colisaoDisparoCanhaoComJogador


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques = atualizaTickDisparoChoque


-- ------------------------------------------------------------------------------------------------------------------
--DISPARO LASER
-- ------------------------------------------------------------------------------------------------------------------
--JOGADORES(Laser)
-- | Atualiza os 'Jogador'es de um 'Estado', considerando apenas os 'DisparoLaser'.
colisaoDisparoLaserComJogador :: Estado -- ^ O 'Estado' anterior.
                              -> Estado -- ^ O 'Estado' atualizado.
colisaoDisparoLaserComJogador (Estado mE jE dE) = Estado mE jogadoresAtualizados dE
                                              where
                                                jogadoresAtualizados = processaJogadoresDisparosLaser dE jE mE

-- | Atualiza os 'Jogador'es, processando os 'DisparoLaser' e verificando os que atingem os 'Jogador'es.
processaJogadoresDisparosLaser :: [Disparo] -- ^ Os 'Disparo's a validar.
                               -> [Jogador] -- ^ Os 'Jogador'es a atualizar.
                               -> Mapa -- ^ O 'Mapa'.
                               -> [Jogador] -- ^ Os 'Jogador'es atualizados.
processaJogadoresDisparosLaser [] jogadores _ = jogadores
processaJogadoresDisparosLaser (DisparoLaser nL posL dL:t) jogadores mapa = processaJogadoresDisparosLaser t jogadoresAtualizados mapa
                                                                            where
                                                                                jogadoresAtualizados = atualizaDisparosLaserColidemComJogadores (DisparoLaser nL posL dL) jogadores mapa
processaJogadoresDisparosLaser (_:t) jogadores mapa = processaJogadoresDisparosLaser t jogadores mapa


-- | Atualiza os 'Jogador'es que foram atingidos pelo 'DisparoLaser'.
atualizaDisparosLaserColidemComJogadores :: Disparo -- ^ O 'DisparoLaser' a considerar.
                                         -> [Jogador] -- ^ Os 'Jogadores' a atualizar.
                                         -> Mapa -- ^ O 'Mapa'.
                                         -> [Jogador] -- ^ Os 'Jogador'es atualizados.
atualizaDisparosLaserColidemComJogadores _ [] _  = []
atualizaDisparosLaserColidemComJogadores (DisparoLaser nL posL dL) (Jogador (l,c) dJ vJ lJ cJ:t) mapa = case jogadorAtingido of
                                                                                                            True -> Jogador (l,c) dJ (vJ-1) lJ cJ:jogadoresAtualizar
                                                                                                            False -> Jogador (l,c) dJ vJ lJ cJ:jogadoresAtualizar
                                                                                                        where
                                                                                                            jogadorAtingido = tanqueAtingidoPorLaser (DisparoLaser nL posL dL) (Jogador (l,c) dJ vJ lJ cJ) mapa
                                                                                                            jogadoresAtualizar = atualizaDisparosLaserColidemComJogadores (DisparoLaser nL posL dL) t mapa


-- | Verifica se um 'Jogador' foi atingido por um 'DisparoLaser'.
tanqueAtingidoPorLaser :: Disparo -- ^ O 'DisparoLaser' a considerar.
                       -> Jogador -- ^ O 'Jogador' a verificar.
                       -> Mapa -- ^ O 'Mapa'.
                       -> Bool -- ^ O resultante da verificação.
tanqueAtingidoPorLaser (DisparoLaser nL (lL,cL) C) (Jogador (l,c) _ vJ _ _) mapa = (cL == c && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) C) (l,c) mapa) || cL == c+1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) C) (l,c+1) mapa) || cL == c-1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) C) (l,c-1) mapa))  && lL >= l && vJ > 0 
tanqueAtingidoPorLaser (DisparoLaser nL (lL,cL) D) (Jogador (l,c) _ vJ _ _) mapa = (lL == l && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) D) (l,c) mapa) || lL == l+1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) D) (l+1,c) mapa) || lL == l-1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) D) (l-1,c) mapa))  && cL <= c && vJ > 0 
tanqueAtingidoPorLaser (DisparoLaser nL (lL,cL) B) (Jogador (l,c) _ vJ _ _) mapa = (cL == c && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) B) (l,c) mapa) || cL == c+1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) B) (l,c+1) mapa) || cL == c-1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) B) (l,c-1) mapa))  && lL <= l && vJ > 0 
tanqueAtingidoPorLaser (DisparoLaser nL (lL,cL) E) (Jogador (l,c) _ vJ _ _) mapa = (lL == l && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) E) (l,c) mapa) || lL == l+1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) E) (l+1,c) mapa) || lL == l-1 && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) E) (l-1,c) mapa))  && cL >= c && vJ > 0 


-- ----------------------------------------------———————————————————------------------------------------------------- 

--DISPAROS(Laser)

-- | Atualiza os 'Disparo's de um dado 'Estado', considerando os 'DisparoLaser'.
colisaoDisparosLaser :: Estado -- ^ O 'Estado' anterior.
                     -> Estado -- ^ O 'Estado' atualizado.
colisaoDisparosLaser (Estado mE jE dE) = Estado mE jE disparosAtualizados
                                       where
                                        disparosAtualizados = atualizaDisparosLaser dE mE


-- | Atualiza os 'Disparo's,considerando os 'DisparoLaser'.
atualizaDisparosLaser :: [Disparo] -- ^ Os 'Disparo's a atualizar.
                      -> Mapa -- ^ O 'Mapa'.
                      -> [Disparo] -- ^ Os 'Disparo's atualizados.
atualizaDisparosLaser [] _ = [] 
atualizaDisparosLaser (DisparoLaser nL posL dL:t) mapa = DisparoLaser nL posL dL : atualizaDisparosLaser disparosAtualizados mapa
                                                         where 
                                                            disparosAtualizados = snd (colisaoEntreDisparos (DisparoLaser nL posL dL) t mapa False)
atualizaDisparosLaser (DisparoCanhao nC posC dC:t) mapa = case colidem of
                                                                True -> atualizaDisparosLaser disparosAtualizados mapa
                                                                False -> DisparoCanhao nC posC dC : atualizaDisparosLaser disparosAtualizados mapa
                                                          where
                                                                colidem = fst (colisaoEntreDisparos (DisparoCanhao nC posC dC) t mapa False)
                                                                disparosAtualizados = snd (colisaoEntreDisparos (DisparoCanhao nC posC dC) t mapa False)
atualizaDisparosLaser (disparo:t) mapa = disparo : atualizaDisparosLaser t mapa

-- | Atualiza os 'Disparos' no caso de ocorrer colisão entre 'DisparoLaser' e 'DisparoCanhao'.
colisaoEntreDisparos :: Disparo -- ^ O 'Disparo' a considerar.
                     -> [Disparo] -- ^ Os 'Disparo's a atualizar.
                     -> Mapa -- ^ O 'Mapa'.
                     -> Bool -- ^ O resultado da verificação de colisões.
                     -> (Bool,[Disparo]) -- ^ A verificação de colisão e os 'Disparo's resultantes.
colisaoEntreDisparos _ [] _ b = (b,[])
colisaoEntreDisparos (DisparoLaser nL posL dL) (DisparoCanhao nC posC dC:t) mapa b = case colidem of
                                                                                        True -> (b,disparosAtualizar)
                                                                                        False -> (b,DisparoCanhao nC posC dC:disparosAtualizar)
                                                                                     where 
                                                                                        colidem = disparosColidem (DisparoLaser nL posL dL) (DisparoCanhao nC posC dC) mapa
                                                                                        disparosAtualizar = snd (colisaoEntreDisparos (DisparoLaser nL posL dL) t mapa b)
colisaoEntreDisparos (DisparoCanhao nC posC dC) (DisparoLaser nL posL dL:t) mapa b = case colidem of
                                                                                            True -> (True,DisparoLaser nL posL dL:t)
                                                                                            False -> (colisao,DisparoLaser nL posL dL:disparosAtualizar)
                                                                                     where
                                                                                        colidem = disparosColidem (DisparoLaser nL posL dL) (DisparoCanhao nC posC dC) mapa
                                                                                        colisao = fst (colisaoEntreDisparos (DisparoCanhao nC posC dC) t mapa b)
                                                                                        disparosAtualizar = snd (colisaoEntreDisparos (DisparoCanhao nC posC dC) t mapa b)
colisaoEntreDisparos disparo (h:t) mapa b = (b,h:disparosAtualizar)
                                          where
                                                disparosAtualizar = snd (colisaoEntreDisparos disparo t mapa b)

-- | Determina a existência de colisão entre um 'DisparoLaser' e um 'DisparoCanhao'.
disparosColidem :: Disparo -- ^ O 'DisparoLaser' a considerar.
                -> Disparo -- ^ O 'DisparoCanhao' a considerar.
                -> Mapa -- ^ O 'Mapa'.
                -> Bool -- ^ O resultado da verificação.
disparosColidem (DisparoLaser nL (lL,cL) C) (DisparoCanhao _ (lC,cC) _) mapa = cL == cC && lL >= lC && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) C) (lC,cC) mapa)
disparosColidem (DisparoLaser nL (lL,cL) D) (DisparoCanhao _ (lC,cC) _) mapa = lL == lC && cL <= lC && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) D) (lC,cC) mapa)
disparosColidem (DisparoLaser nL (lL,cL) B) (DisparoCanhao _ (lC,cC) _) mapa = cL == cC && lL <= lC && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) B) (lC,cC) mapa)
disparosColidem (DisparoLaser nL (lL,cL) E) (DisparoCanhao _ (lC,cC) _) mapa = lL == lC && cL >= lC && not (existeBlocoIndestrutivel (DisparoLaser nL (lL,cL) E) (lC,cC) mapa)


-- | Determina se um 'DisparoLaser' se expande até uma 'Posicao'.
existeBlocoIndestrutivel :: Disparo -- ^ O 'DisparoLaser' a considerar.
                         -> Posicao -- ^ A 'Posicao' a considerar.
                         -> Mapa -- ^ O 'Mapa'.
                         -> Bool -- ^ O resultado da verificação.
existeBlocoIndestrutivel(DisparoLaser _ posL dL) = pecaEntrePosicoes posL dL posV
                                                           where
                                                                posV = posicaoVizinha posL dL


-- | Determina se exite algum 'Bloco Indestrutivel' entre duas posicções.
pecaEntrePosicoes:: Posicao -- ^ A 'Posicao' do 'DisparoLaser'.
                 -> Direcao -- ^ A 'Direcao' do 'DisparoLaser'.
                 -> Posicao -- ^ A 'Posicao' vizinha ao 'DisparoLaser'
                 -> Posicao -- ^ A 'Posicao' a verificar.
                 -> Mapa -- ^ O 'Mapa'.
                 -> Bool -- ^ O resultado da verificação.
pecaEntrePosicoes posL dL posV pos mapa | posL == pos || posV == pos = False
                                        | peca1  == Bloco Indestrutivel || peca2  == Bloco Indestrutivel = True
                                        | otherwise = pecaEntrePosicoes proxPos dL proxPosV pos mapa
                                        where 
                                            peca1 = encontraPosicaoMatriz posL mapa
                                            peca2 = encontraPosicaoMatriz posV mapa
                                            proxPos = proximaPosicao posL dL
                                            proxPosV = proximaPosicao posV dL

-- ------------------------------------------------------------------------------------------------------------------

--MAPA(Laser)
-- | Atualiza o 'Mapa' e os 'Disparo's de um 'Estado', considerando apenas os 'DisparoLaser'.
colisaoDisparoLaserComMapa :: Estado -- ^ O 'Estado' anterior.
                           -> Estado -- ^ O 'Estado' atualizado
colisaoDisparoLaserComMapa (Estado mE jE dE) = Estado mapaAtualizado jE disparosAtualizados 
                                             where
                                                mapaAtualizado = fst (processaMapaDisparosLaser mE dE)
                                                disparosAtualizados = snd (processaMapaDisparosLaser mE dE)

-- | Processa os 'DisparoLaser' que colidem com o 'Mapa'.
processaMapaDisparosLaser :: Mapa -- ^ O 'Mapa' a considerar.
                          -> [Disparo] -- ^ Os 'Disparo's a considerar.
                          -> (Mapa,[Disparo]) -- ^ O 'Mapa' e os 'Disparo's resultantes. 
processaMapaDisparosLaser mapa [] = (mapa,[])
processaMapaDisparosLaser mapa (DisparoLaser _ posL dL:t) = (mapaAtualizar,disparosAtualizar)
                                                             where
                                                                    mapaAtualizado = atualizaMapaDisparoLaser mapa posL dL
                                                                    mapaAtualizar = fst (processaMapaDisparosLaser mapaAtualizado t)
                                                                    disparosAtualizar = snd (processaMapaDisparosLaser mapaAtualizado t)
processaMapaDisparosLaser mapa (disparo:t) = (atualizarMapa,disparo:atualizarDisparos)
                                           where
                                                atualizarMapa = fst (processaMapaDisparosLaser mapa t)
                                                atualizarDisparos = snd (processaMapaDisparosLaser mapa t)

-- | Atualiza o 'Mapa'.
atualizaMapaDisparoLaser :: Mapa -- ^ O 'Mapa'.
                         -> Posicao -- ^ A 'Posicao' do 'DisparoLaser'.
                         -> Direcao -- ^ A 'Direcao' do 'DisparoLaser'.
                         -> Mapa -- ^ O 'Mapa' resultante.
atualizaMapaDisparoLaser mapa posL dL = case progride of
                                        True -> atualizaMapaDisparoLaser mapaAtualizado proxPos dL
                                        False -> mapaAtualizado
                                      where
                                        posV = posicaoVizinha posL dL
                                        progride = fst (atualizaPecasDisparoLaser posL posV mapa)
                                        mapaAtualizado = snd (atualizaPecasDisparoLaser posL posV mapa)
                                        proxPos = proximaPosicao posL dL 

 

-- | Determina se um 'DisparoLaser' progride e atualiza o 'Mapa'.
atualizaPecasDisparoLaser :: Posicao -- ^ A 'Posicao' do 'DisparoLaser'.
                          -> Posicao -- ^ A 'Posicao' vizinha do 'DisparoLaser'.
                          -> Mapa -- ^ O 'Mapa'.
                          -> (Bool,Mapa) -- ^  A verificação da progressão do 'Disparo' e 'Mapa' atualizado.
atualizaPecasDisparoLaser pos1 pos2 mapa | posicao1 == Vazia               && posicao2 == Vazia               = (True,mapa)
                                         | posicao1 == Vazia               && posicao2 == Bloco Destrutivel   = (True,atualizaPosicaoMatriz pos2 Vazia mapa)
                                         | posicao1 == Vazia               && posicao2 == Bloco Indestrutivel = (False,mapa)
                                         | posicao1 == Bloco Destrutivel   && posicao2 == Vazia               = (True,atualizaPosicaoMatriz pos1 Vazia mapa)
                                         | posicao1 == Bloco Destrutivel   && posicao2 == Bloco Destrutivel   = (True,atualizaPosicaoMatriz pos1 Vazia (atualizaPosicaoMatriz pos2 Vazia mapa))
                                         | posicao1 == Bloco Destrutivel   && posicao2 == Bloco Indestrutivel = (False,atualizaPosicaoMatriz pos1 Vazia mapa)
                                         | posicao1 == Bloco Indestrutivel && posicao2 == Vazia               = (False,mapa)
                                         | posicao1 == Bloco Indestrutivel && posicao2 == Bloco Destrutivel   = (False,atualizaPosicaoMatriz pos2 Vazia mapa)
                                         | otherwise                                                          = (False,mapa)
                                         where
                                            posicao1 = encontraPosicaoMatriz pos1 mapa
                                            posicao2 = encontraPosicaoMatriz pos2 mapa 


-- ------------------------------------------————————————————————————————————————————————————————-——--——-—-——-—------
-- DISPARO CANHAO
-- ------------------------------------------————————————————————————————————————————————————————-——--——-—-——-—------

-- JOGADORES (Canhao)


-- | Atualiza os 'Jogador'es de um 'Estado', considerando os 'DisparoCanhao'.
colisaoDisparoCanhaoComJogador :: Estado -- ^ O 'Estado' anterior.
                               -> Estado -- ^ O 'Estado' atualizado.
colisaoDisparoCanhaoComJogador (Estado mE jE dE) = Estado mE jogadoresAtualizados disparosAtualizados
                                                 where
                                                    disparosAtualizados = fst (processaJogadoresDisparosCanhao dE jE)
                                                    jogadoresAtualizados = snd (processaJogadoresDisparosCanhao dE jE)


-- | Atualiza os 'Jogador'es, processando os 'Disparo's que atingem os 'Jogador'es.
processaJogadoresDisparosCanhao :: [Disparo] -- ^ Os 'Disparo's a atualizar.
                                -> [Jogador] -- ^ Os 'Jogador'es a atualizar.
                                -> ([Disparo],[Jogador]) -- ^ Os 'Disparo's e os 'Jogador'es atualizados.
processaJogadoresDisparosCanhao [] jogadores = ([],jogadores)
processaJogadoresDisparosCanhao (DisparoCanhao n pos d:t) jogadores = case explode of 
                                                                                True -> (disparosAtualizar,jogadoresAtualizar)
                                                                                False -> (DisparoCanhao n pos d:disparosAtualizar,jogadoresAtualizar)
                                                                      where
                                                                            explode = fst (atualizaDisparosCanhaoColidemComJogadores False (DisparoCanhao n pos d) jogadores)
                                                                            jogadoresAtualizados = snd (atualizaDisparosCanhaoColidemComJogadores False (DisparoCanhao n pos d) jogadores)
                                                                            disparosAtualizar = fst (processaJogadoresDisparosCanhao t jogadoresAtualizados)
                                                                            jogadoresAtualizar = snd (processaJogadoresDisparosCanhao t jogadoresAtualizados)
processaJogadoresDisparosCanhao (disparo:t) jogadores = (disparo:disparosAtualizar,jogadoresAtualizar)
                                                      where
                                                        disparosAtualizar = fst (processaJogadoresDisparosCanhao t jogadores)
                                                        jogadoresAtualizar = snd (processaJogadoresDisparosCanhao t jogadores)




-- | Determina se um 'DisparoCanhao' atinge algum 'Jogador' atualizando-os.
atualizaDisparosCanhaoColidemComJogadores :: Bool -- ^ O indicador se um 'Jogador' foi atingido.
                                          -> Disparo -- ^ O 'DisparoCanhao'.
                                          -> [Jogador] -- ^ Os 'Jogador'es.
                                          -> (Bool,[Jogador]) -- ^ O resultado da verificação e os 'Jogador'es atualizados.
atualizaDisparosCanhaoColidemComJogadores b _ [] = (b,[])
atualizaDisparosCanhaoColidemComJogadores b (DisparoCanhao n pos d) (Jogador (l,c) dJ vJ lJ cJ:t)  = case jogadorAtingido of
                                                                                                        True -> (True, Jogador (l,c) dJ (vJ-1) lJ cJ:t)
                                                                                                        False -> (colisao,Jogador (l,c) dJ vJ lJ cJ:jogadoresAtualizar)
                                                                                                    where
                                                                                                        proxPos = proximaPosicaoCanhao pos d
                                                                                                        jogadorAtingido = tanqueAtingidoPorCanhao proxPos d (Jogador (l,c) dJ vJ lJ cJ)
                                                                                                        colisao = fst (atualizaDisparosCanhaoColidemComJogadores b (DisparoCanhao n pos d) t) 
                                                                                                        jogadoresAtualizar = snd (atualizaDisparosCanhaoColidemComJogadores b (DisparoCanhao n pos d) t) 



-- | Verifica se um 'DisparoCanhao' atinge um 'Jogador'.
tanqueAtingidoPorCanhao :: Posicao -- ^ A 'Posicao' do 'DisparoCanhao'.
                        -> Direcao -- ^ A 'Direcao' do 'DisparoCanhao'.
                        -> Jogador -- ^ O 'Jogador'.
                        -> Bool -- ^ O resultado da verificação.
tanqueAtingidoPorCanhao (lC,cC) C (Jogador (l,c) _ vJ _ _) = (cC == c  || cC == c+1  || cC == c-1) && lC == l+1 && vJ > 0 
tanqueAtingidoPorCanhao (lC,cC) D (Jogador (l,c) _ vJ _ _) = (lC == l  || lC == l+1  || lC == l-1) && cC == c && vJ > 0 
tanqueAtingidoPorCanhao (lC,cC) B (Jogador (l,c) _ vJ _ _) = (cC == c  || cC == c+1  || cC == c-1) && lC == l && vJ > 0 
tanqueAtingidoPorCanhao (lC,cC) E (Jogador (l,c) _ vJ _ _) = (lC == l  || lC == l+1  || lC == l-1) && cC == c+1 && vJ > 0 


-- ----------------------------------------------———————————————————-------------------------------------------------

-- DISPAROS(Canhao)


-- | Atualiza os 'Disparo's de um dado 'Estado', considerando os 'DisparoCanhao'.
colisaoDisparoCanhao :: Estado -- ^ O 'Estado' anterior.
                     -> Estado -- ^ O 'Estado' atualizado.
colisaoDisparoCanhao (Estado mE jE dE) = Estado mE jE disparosAtualizados
                                        where 
                                            disparosAtualizados = atualizaDisparosCanhao dE


-- | Atualiza os 'Disparo's,considerando os 'DisparoCanhao'.
atualizaDisparosCanhao :: [Disparo] -- ^ Os 'Disparo's a atualizar.
                       -> [Disparo] -- ^ Os 'Disparo's a atualizados.
atualizaDisparosCanhao [] = []
atualizaDisparosCanhao (DisparoCanhao n pos d:t) = case explode of
                                                        True -> disparosAtualizar
                                                        False -> DisparoCanhao n pos d : disparosAtualizar
                                                    where
                                                        explode = fst (colisaoCanhaoEntreDisparos (DisparoCanhao n pos d) False t)
                                                        disparosAtualizados = snd (colisaoCanhaoEntreDisparos (DisparoCanhao n pos d) False t)
                                                        disparosAtualizar =  atualizaDisparosCanhao disparosAtualizados
atualizaDisparosCanhao (disparo:t) = disparo:atualizaDisparosCanhao t


-- | Atualiza os 'Disparos' no caso de ocorrer colisão entre 'DisparoCanhao'.
colisaoCanhaoEntreDisparos :: Disparo -- ^ O 'Disparo' a considerar.
                           -> Bool -- ^ O resultado da verificação de colisões.
                           -> [Disparo] -- ^ Os 'Disparo's a atualizar.
                           -> (Bool,[Disparo]) -- ^ A verificação de colisão e os 'Disparo's resultantes.
colisaoCanhaoEntreDisparos _ b [] = (b,[])
colisaoCanhaoEntreDisparos (DisparoCanhao n pos d) b (DisparoCanhao nC posC dC:t) = case colidem of
                                                                                            True -> (True,disparosAtualizar)
                                                                                            False -> (b,DisparoCanhao nC posC dC:disparosAtualizar)
                                                                                        where 
                                                                                            colidem = disparosColidemCanhao (DisparoCanhao n pos d) (DisparoCanhao nC posC dC)
                                                                                            disparosAtualizar = snd (colisaoCanhaoEntreDisparos (DisparoCanhao n pos d) b t)
colisaoCanhaoEntreDisparos disparo b (h:t) = (b,h:disparosAtualizar)
                                            where
                                                disparosAtualizar = snd (colisaoCanhaoEntreDisparos disparo b t)


-- | Verifica se dois 'DisparoCanhao' colidem.
disparosColidemCanhao :: Disparo -- ^ O 'DisparoCanhao' a considerar.
                      -> Disparo -- ^ O 'DisparoCanhao' a considerar.
                      -> Bool -- ^ O resultado da verificação de colisão.
disparosColidemCanhao (DisparoCanhao _ (l,c) d) (DisparoCanhao _ (lC,cC) dC) = l == lC && c == cC-1 && d == E && dC == D  ||  l==lC && c == cC+1 && d == D && dC == E   ||   l == lC-1 && c == cC && d == C && dC == B   ||   l == lC+1 && c == cC && d == B && dC == C   ||  l == lC && c == cC

-- ------------------------------------------------------------------------------------------------------------------

--Mapa(Canhao)

-- | Atualiza o 'Mapa' e os 'Disparo's de um 'Estado', considerando apenas os 'DisparoCanhao'.
colisaoDisparoCanhaoComMapa :: Estado -- ^ O 'Estado' anterior.
                            -> Estado -- ^ O 'Estado' atualizado
colisaoDisparoCanhaoComMapa (Estado mE jE dE) = Estado mapaAtualizado jE disparosAtualizados
                                                where
                                                    mapaAtualizado = fst (processaMapaDisparosCanhao mE dE)
                                                    disparosAtualizados = snd (processaMapaDisparosCanhao mE dE)


-- | Processa os 'DisparoCanhao' que colidem com o 'Mapa'.
processaMapaDisparosCanhao :: Mapa -- ^ O 'Mapa' a considerar.
                           -> [Disparo] -- ^ Os 'Disparo's a considerar.
                           -> (Mapa,[Disparo]) -- ^ O 'Mapa' e os 'Disparo's resultantes.
processaMapaDisparosCanhao mapa [] = (mapa,[])
processaMapaDisparosCanhao mapa (DisparoCanhao n pos d:t) = case explode of
                                                                True -> (mapaAtualizar,disparosAtualizar)
                                                                False -> (mapaAtualizar,DisparoCanhao n pos d:disparosAtualizar)
                                                           where
                                                                proxPos = proximaPosicaoCanhao pos d
                                                                posV = posicaoVizinha proxPos d
                                                                explode = fst (atualizaPecasDisparoCanhao proxPos posV mapa) 
                                                                mapaAtualizado = snd (atualizaPecasDisparoCanhao proxPos posV mapa)  
                                                                mapaAtualizar = fst (processaMapaDisparosCanhao mapaAtualizado t)
                                                                disparosAtualizar = snd (processaMapaDisparosCanhao mapaAtualizado t)
processaMapaDisparosCanhao mapa (disparo:t) = (mapaAtualizar,disparo:disparosAtualizar) 
                                            where
                                                mapaAtualizar = fst (processaMapaDisparosCanhao mapa t)
                                                disparosAtualizar = snd (processaMapaDisparosCanhao mapa t)



-- | Determina se um 'DisparoCanhao' explode e atualiza o 'Mapa'.
atualizaPecasDisparoCanhao :: Posicao -- ^ A 'Posicao' do 'DisparoCanhao'.
                           -> Posicao -- ^ A 'Posicao' vizinha do 'DisparoCanhao'.
                           -> Mapa -- ^ O 'Mapa'.
                           -> (Bool,Mapa) -- ^  A verificação da explosão do 'Disparo' e 'Mapa' atualizado.
atualizaPecasDisparoCanhao pos posV mapa | peca1 == Vazia && peca2 == Vazia = (False,mapa) 
                                          | peca1 == Vazia && peca2 == Bloco Destrutivel = (True,atualizaPosicaoMatriz posV Vazia mapa)
                                          | peca1 == Vazia && peca2 == Bloco Indestrutivel = (True,mapa)
                                          | peca1 == Bloco Destrutivel && peca2 == Vazia = (True,atualizaPosicaoMatriz pos Vazia mapa)
                                          | peca1 == Bloco Destrutivel && peca2 == Bloco Destrutivel = (True, atualizaPosicaoMatriz pos Vazia (atualizaPosicaoMatriz posV Vazia mapa))
                                          | peca1 == Bloco Destrutivel && peca2 == Bloco Indestrutivel = (True,atualizaPosicaoMatriz pos Vazia mapa)
                                          | peca1 == Bloco Indestrutivel && peca2 == Vazia = (True, mapa)
                                          | peca1 == Bloco Indestrutivel && peca2 == Bloco Destrutivel = (True,atualizaPosicaoMatriz posV Vazia mapa)
                                          | otherwise = (True,mapa)
                                          where
                                            peca1 = encontraPosicaoMatriz pos mapa
                                            peca2 = encontraPosicaoMatriz posV mapa
-- ----------------------------------------------———————————————————-------------------------------------------------

-- POSICAO SEGUINTE (Canhao)


-- | Atualiza o 'Estado', colocando os 'DisparoCanhao' na 'Posicao' seguinte.
atualizaEstadoCanhao :: Estado -- ^ O 'Estado' anterior.
                     -> Estado -- ^ O 'Estado' atualizado.
atualizaEstadoCanhao (Estado mE jE dE) = Estado mE jE disparosAtualizados
                                       where
                                            disparosAtualizados = avancaPosicaoDisparoCanhao dE


-- | Atualiza os 'DisparoCanhao' colocando-os na 'Posicao' seguinte.
avancaPosicaoDisparoCanhao :: [Disparo] -- ^ Os 'Disparos'.
                           -> [Disparo] -- ^ Os 'DisparoCanhao' na 'Posicao' seguinte.
avancaPosicaoDisparoCanhao [] = []
avancaPosicaoDisparoCanhao (DisparoCanhao n pos d:t) = DisparoCanhao n proxPos d : avancaPosicaoDisparoCanhao t
                                                       where
                                                            proxPos = proximaPosicao pos d
avancaPosicaoDisparoCanhao (disparo:t) = disparo : avancaPosicaoDisparoCanhao t


--  --------------------------------——————————————————------------—————————————-——————————————------------------------
-- DisparoChoque 
-- -------------------------------------------------------------------------------------------------------------------

-- | Atualiza o 'Estado', considerando os 'DisparoChoque'.
atualizaTickDisparoChoque :: Estado-- ^ O 'Estado' anterior.
                          -> Estado -- ^ O 'Estado' atualizado.
atualizaTickDisparoChoque (Estado mE jE dE) = Estado mE jE disparosAtualizados
                                            where
                                                disparosAtualizados = atualizaDisparoChoque dE

-- | Atualiza os 'DisparoChoque' decrementando o seu 'tempoDisparo'.
atualizaDisparoChoque :: [Disparo] -- ^ Os 'Disparo's a considerar.
                      -> [Disparo] -- ^ Os 'Disparo's atualizados.
atualizaDisparoChoque [] = []
atualizaDisparoChoque (DisparoChoque n tD:t) = case tD > 0 of 
                                                    True -> DisparoChoque n (tD-1) : atualizaDisparoChoque t
                                                    False -> atualizaDisparoChoque t
atualizaDisparoChoque (disparo:t) = disparo : atualizaDisparoChoque t



-- --------------------------------——————————————————------------—————————————-——————————————------------------------

--  GLOBAIS

-- | Determina a 'Posicao' vizinha à 'Posicao' de um 'Disparo'.
posicaoVizinha :: Posicao -- ^ A 'Posicao' do 'Disparo'.
               -> Direcao -- ^ A 'Direcao' do 'Disparo'.
               -> Posicao -- ^ A 'Posicao' vizinha resultante.
posicaoVizinha (l,c) d = case d == C || d == B of
                            True -> (l,c+1)
                            False -> (l+1,c)


-- | Determina a próxima 'Posicao' em função de uma 'Direcao'.
proximaPosicao :: Posicao -- ^ A 'Posicao' do 'Disparo'.
               -> Direcao -- ^ A 'Direcao' do 'Disparo'.
               -> Posicao -- ^ A próxima 'Posicao' resultante.
proximaPosicao (l,c) C = (l-1,c)
proximaPosicao (l,c) D = (l,c+1)
proximaPosicao (l,c) B = (l+1,c)
proximaPosicao (l,c) E = (l,c-1)

-- | Determina a próxima 'Posicao' do 'DisparoCanhao' em função de uma 'Direcao'.
proximaPosicaoCanhao :: Posicao -- ^ A 'Posicao' do 'Disparo'.
                     -> Direcao -- ^ A 'Direcao' do 'Disparo'.
                     -> Posicao -- ^ A próxima 'Posicao' resultante.
proximaPosicaoCanhao (l,c) C = (l,c)
proximaPosicaoCanhao (l,c) D = (l,c+1)
proximaPosicaoCanhao (l,c) B = (l+1,c)
proximaPosicaoCanhao (l,c) E = (l,c) 
