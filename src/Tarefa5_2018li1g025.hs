-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where


import LI11819
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa2_2018li1g025
import Tarefa3_2018li1g025
import Tarefa4_2018li1g025
import Tarefa6_2018li1g025


-- * Funções da Tarefa 5

-- | Tamanho das 'Peca's.
tamanhoImagemPeca::Int
tamanhoImagemPeca = 500

-- | Tamanho dos Menus.
tamanhoImagemMenu :: Int
tamanhoImagemMenu = 1000

-- | Estado do jogo.
data State = State{
    estado  :: Estado,
    jogadas :: Jogadas,
    imagens :: Imagens,
    posMenu :: PosMenu,
    mode :: Int,
    players :: Int,
    winSize :: (Int, Int)
}

-- | Posição do menu em que o jogo está.
data PosMenu = Play 
             | Pause 
             | MenuInicial
             | MenuModeB
             | MenuModeP 
             | MenuPlayer1 
             | MenuPlayer2 
             | MenuPlayer3 
             | MenuPlayer4 
             | MenuMapa1
             | MenuMapa2
             | MenuMapa3 
             | MenuMapa4
             | GameOver deriving Eq

-- | Jogadas de cada jogador.
data Jogadas = Jogadas{
    j1 :: Maybe Jogada,
    j2 :: Maybe Jogada,
    j3 :: Maybe Jogada,
    j4 :: Maybe Jogada
}

-- | Jogadas default, não fazer nada.
jogadasDefault :: Jogadas
jogadasDefault = Jogadas Nothing Nothing Nothing Nothing

-- | Todas as 'Picture' a utilizar.
data Imagens = Imagens{
    vazio :: Picture,
    destrutivel :: Picture,
    indestrutivel :: Picture,
    tank :: Picture,
    canhao :: Picture,
    choque :: Picture,
    menuInicial :: Picture,
    menuModeB :: Picture,
    menuModeP :: Picture,
    menuPlayer1 :: Picture,
    menuPlayer2 :: Picture,
    menuPlayer3 :: Picture,
    menuPlayer4 :: Picture,
    menuMapa1 :: Picture,
    menuMapa2 :: Picture,
    menuMapa3 :: Picture,
    menuMapa4 :: Picture,
    pause :: Picture,
    gameOver :: Picture
}

-- | Todos os 'Mapa's a utilizar.
mapas :: Int -> String
mapas 1 = "19,40;78V2I10V10I4V4I8V2I10V10I4V4I8V2I14V2I10V2I8V2I14V2I10V2I8V2I14V2I10V2I8V2I14V2I10V2I8V2I14V2I10V2I8V2I14V2I10V2I8V2I14V2I10V2I8V2I14V2I10V2I8V2I14V2I10V2I8V10I2V10I2V10I4V10I2V10I2V10I78V;P1,1,D,6,3,180P16,1,C,6,3,180P1,37,B,6,3,180P16,37,E,6,3,180;"
mapas 2 = "17,23;2V17I4V17I4V17I4V17I2V2D17V4D17V4D2V13D2V4D2V13D2V4D2V13D2V4D17V4D17V2D2V17I4V17I4V17I4V17I2V;P1,1,B,6,3,180P14,1,C,6,3,180P1,20,B,6,3,180P14,20,C,6,3,180;"
mapas 3 = "13,24;2V2D6V2D6V2D4V2D6V2D6V2D4V8I2V8I2V9DI2VI18DI2VI9D2V5I2DI2VI2D5I2V9DI2VI18DI2VI9D2V8I2V8I4V2D6V2D6V2D4V2D6V2D6V2D2V;P1,1,D,6,3,180P10,1,C,6,3,180P1,21,B,6,3,180P10,21,E,6,3,180;"
mapas 4 = "21,34;15V3I29V3I29V3I18V2I2V5I2V3I2V4I2V2I8V2I2V5I2V3D2V4I2V2I8V2I9V3D8V2I4V4D2I9V3D8V2I8D2I9VDID8V2I8D2I9VDID8V2I8D2I9VDID8V2I8D2I9VDID8V2I8D2I9V3D8V2I8D2I9V3D8V2I4D4V2I9V3D8V2I8V2I2V5I2V3D2V4I2V2I8V2I2V5I2V3I2V4I2V2I19V3I29V3I29V3I14V;P1,1,D,6,3,180P18,1,C,6,3,180P1,31,B,6,3,180P18,31,E,6,3,180;"

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do inicio <- estadoInicial
          joga inicio


-- | Função que controla o jogo.
joga :: State -> IO()
joga inicio = play
      (InWindow  "Squares" (1280,720) (0,0) )  -- Janela onde vai correr o jogo      
      (greyN 1)                                -- Cor do fundo da janela.
      60                                       -- Frame Rate
      inicio                                   -- Fundo inicial do jogo.
      desenhaEstado                            -- Desenha o Estado do jogo.
      reageEvento                              -- Reage a um Evento.
      reageTempo                               -- Reage ao passar do Tempo.

-- | Função que devolve uma 'Picture' a partir de um 'State'.
estadoInicial :: IO State
estadoInicial = do  vazio          <- loadBMP "sprites/pecas/floor.bmp"
                    destrutivel    <- loadBMP "sprites/pecas/destrutivel.bmp"
                    indestrutivel  <- loadBMP "sprites/pecas/indestrutivel.bmp"
                    tank           <- loadBMP "sprites/tank.bmp"
                    canhao         <- loadBMP "sprites/disparos/canhao.bmp"
                    choque         <- loadBMP "sprites/disparos/choque.bmp"
                    menuInicial    <- loadBMP "sprites/menus/menu.bmp"
                    menuModeB      <- loadBMP "sprites/menus/menuModeB.bmp"
                    menuModeP      <- loadBMP "sprites/menus/menuModeP.bmp"
                    menuPlayer1    <- loadBMP "sprites/menus/menuPlayer1.bmp"
                    menuPlayer2    <- loadBMP "sprites/menus/menuPlayer2.bmp"
                    menuPlayer3    <- loadBMP "sprites/menus/menuPlayer3.bmp"
                    menuPlayer4    <- loadBMP "sprites/menus/menuPlayer4.bmp"
                    menuMapa1      <- loadBMP "sprites/menus/menuMapa1.bmp"
                    menuMapa2      <- loadBMP "sprites/menus/menuMapa2.bmp"
                    menuMapa3      <- loadBMP "sprites/menus/menuMapa3.bmp"
                    menuMapa4      <- loadBMP "sprites/menus/menuMapa4.bmp"
                    pause          <- loadBMP "sprites/menus/pause.bmp"
                    gameOver       <- loadBMP "sprites/menus/gameOver.bmp"
                    let imageRecord = Imagens vazio destrutivel indestrutivel tank canhao choque menuInicial menuModeB menuModeP menuPlayer1 menuPlayer2 menuPlayer3 menuPlayer4 menuMapa1 menuMapa2 menuMapa3 menuMapa4 pause gameOver
                    return (State (descomprime $ mapas 1) jogadasDefault imageRecord MenuInicial 1 1 (1280,720))

-- ** Reagir a um 'Event'
-- | Função que altera o 'State' quando acontece um evento.
reageEvento :: Event -> State -> State
reageEvento (EventResize size) s = s {winSize = size}
reageEvento e s | posMenu s == Pause = reageEventoPausa e s
                | posMenu s == Play  = reageEventoJogo e s
                | otherwise = reageEventoMenu e s


-- *** Reagir a um 'Event' no Menu
-- | Função que, quando o jogador está no Menu, altera o 'State' do jogo quando ocorre um evento.
reageEventoMenu :: Event -> State -> State
reageEventoMenu (EventKey (SpecialKey KeyEnter)  Down _ _) s | posMenu s == MenuInicial = s {posMenu = MenuModeB}
                                                             | posMenu s == MenuModeB = s {mode = 1, posMenu = MenuPlayer1}
                                                             | posMenu s == MenuModeP = s {mode = 0, posMenu = MenuPlayer1}
                                                             | posMenu s == MenuPlayer1 && mode s == 1 = s {players = 1, posMenu = MenuMapa1}
                                                             | posMenu s == MenuPlayer2 = s {players = 2, posMenu = MenuMapa1}
                                                             | posMenu s == MenuPlayer3 = s {players = 3, posMenu = MenuMapa1}
                                                             | posMenu s == MenuPlayer4 && mode s == 0 = s {players = 4, posMenu = MenuMapa1}
                                                             | posMenu s == MenuMapa1 = numeroplayers s{estado = descomprime $ mapas 1, posMenu = Play}
                                                             | posMenu s == MenuMapa2 = numeroplayers s{estado = descomprime $ mapas 2, posMenu = Play}
                                                             | posMenu s == MenuMapa3 = numeroplayers s{estado = descomprime $ mapas 3, posMenu = Play}
                                                             | posMenu s == MenuMapa4 = numeroplayers s{estado = descomprime $ mapas 4, posMenu = Play}
                                                             | posMenu s == GameOver = s{posMenu = MenuInicial}

reageEventoMenu (EventKey (SpecialKey KeyUp)   Down _ _) s   | posMenu s == MenuPlayer3 = s {posMenu = MenuPlayer1}
                                                             | posMenu s == MenuPlayer4 = s {posMenu = MenuPlayer2}
                                                             | posMenu s == MenuModeP = s {posMenu = MenuModeB}
                                                             | posMenu s == MenuMapa3 = s {posMenu = MenuMapa1}
                                                             | posMenu s == MenuMapa4 = s {posMenu = MenuMapa2}

reageEventoMenu (EventKey (SpecialKey KeyDown)   Down _ _) s | posMenu s == MenuPlayer1 = s {posMenu = MenuPlayer3}
                                                             | posMenu s == MenuPlayer2 = s {posMenu = MenuPlayer4}
                                                             | posMenu s == MenuModeB = s {posMenu = MenuModeP}
                                                             | posMenu s == MenuMapa1 = s {posMenu = MenuMapa3}
                                                             | posMenu s == MenuMapa2 = s {posMenu = MenuMapa4}


reageEventoMenu (EventKey (SpecialKey KeyLeft)   Down _ _) s | posMenu s == MenuPlayer2 = s {posMenu = MenuPlayer1}
                                                             | posMenu s == MenuPlayer4 = s {posMenu = MenuPlayer3}
                                                             | posMenu s == MenuMapa2 = s {posMenu = MenuMapa1}
                                                             | posMenu s == MenuMapa4 = s {posMenu = MenuMapa3}

reageEventoMenu (EventKey (SpecialKey KeyRight)  Down _ _) s | posMenu s == MenuPlayer1 = s {posMenu = MenuPlayer2}
                                                             | posMenu s == MenuPlayer3 = s {posMenu = MenuPlayer4}
                                                             | posMenu s == MenuMapa1 = s {posMenu = MenuMapa2}
                                                             | posMenu s == MenuMapa3 = s {posMenu = MenuMapa4}

reageEventoMenu (EventKey (SpecialKey KeyTab)  Down _ _) s | posMenu s == MenuModeB = s {posMenu = MenuInicial}
                                                           | posMenu s == MenuModeP = s {posMenu = MenuInicial}
                                                           | posMenu s == MenuPlayer1 = s {posMenu = MenuModeB}
                                                           | posMenu s == MenuPlayer2 = s {posMenu = MenuModeB}
                                                           | posMenu s == MenuPlayer3 = s {posMenu = MenuModeB}
                                                           | posMenu s == MenuPlayer4 = s {posMenu = MenuModeB}
                                                           | posMenu s == MenuMapa1 = s{posMenu = MenuPlayer1}
                                                           | posMenu s == MenuMapa2 = s{posMenu = MenuPlayer1}
                                                           | posMenu s == MenuMapa3 = s{posMenu = MenuPlayer1}
                                                           | posMenu s == MenuMapa4 = s{posMenu = MenuPlayer1}

reageEventoMenu _ s = s


-- *** Reagir a um 'Event' no Menu Pausa
-- | Função que, quando o jogador está no Menu Pausa, altera o 'State' do jogo quando ocorre um evento.
reageEventoPausa :: Event -> State -> State
reageEventoPausa (EventKey (Char 'p')  Down _ _) s = s {posMenu = Play}
reageEventoPausa _ s = s


-- *** Reagir a um 'Event' no Jogo
-- | Função que, quando o jogador está no Jogo, altera o 'State' do jogo quando um evento ocorre
reageEventoJogo :: Event -> State -> State
reageEventoJogo (EventKey (Char 'w')  Down _ _) s = s {jogadas = (jogadas s) {j1 = Just $ Movimenta C}}
reageEventoJogo (EventKey (Char 's')  Down _ _) s = s {jogadas = (jogadas s) {j1 = Just $ Movimenta B}}
reageEventoJogo (EventKey (Char 'a')  Down _ _) s = s {jogadas = (jogadas s) {j1 = Just $ Movimenta E}}
reageEventoJogo (EventKey (Char 'd')  Down _ _) s = s {jogadas = (jogadas s) {j1 = Just $ Movimenta D}}
reageEventoJogo (EventKey (Char '1')  Down _ _) s = s {jogadas = (jogadas s) {j1 = Just $ Dispara Canhao}}
reageEventoJogo (EventKey (Char '2')  Down _ _) s = s {jogadas = (jogadas s) {j1 = Just $ Dispara Laser}}
reageEventoJogo (EventKey (Char '3')  Down _ _) s = s {jogadas = (jogadas s) {j1 = Just $ Dispara Choque}}
reageEventoJogo (EventKey (Char 't')  Down _ _) s | players s >= 2 = s {jogadas = (jogadas s) {j2 = Just $ Movimenta C}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char 'g')  Down _ _) s | players s >= 2 = s {jogadas = (jogadas s) {j2 = Just $ Movimenta B}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char 'f')  Down _ _) s | players s >= 2 = s {jogadas = (jogadas s) {j2 = Just $ Movimenta E}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char 'h')  Down _ _) s | players s >= 2 = s {jogadas = (jogadas s) {j2 = Just $ Movimenta D}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '4')  Down _ _) s | players s >= 2 = s {jogadas = (jogadas s) {j2 = Just $ Dispara Canhao}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '5')  Down _ _) s | players s >= 2 = s {jogadas = (jogadas s) {j2 = Just $ Dispara Laser}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '6')  Down _ _) s | players s >= 2 = s {jogadas = (jogadas s) {j2 = Just $ Dispara Choque}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char 'i')  Down _ _) s | players s >= 3 = s {jogadas = (jogadas s) {j3 = Just $ Movimenta C}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char 'k')  Down _ _) s | players s >= 3 = s {jogadas = (jogadas s) {j3 = Just $ Movimenta B}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char 'j')  Down _ _) s | players s >= 3 = s {jogadas = (jogadas s) {j3 = Just $ Movimenta E}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char 'l')  Down _ _) s | players s >= 3 = s {jogadas = (jogadas s) {j3 = Just $ Movimenta D}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '7')  Down _ _) s | players s >= 3 = s {jogadas = (jogadas s) {j3 = Just $ Dispara Canhao}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '8')  Down _ _) s | players s >= 3 = s {jogadas = (jogadas s) {j3 = Just $ Dispara Laser}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '9')  Down _ _) s | players s >= 3 = s {jogadas = (jogadas s) {j3 = Just $ Dispara Choque}}
                                                  | otherwise = s

reageEventoJogo (EventKey (SpecialKey KeyUp)     Down _ _) s | players s == 4 = s {jogadas = (jogadas s) {j4 = Just $ Movimenta C}}
                                                             | otherwise = s

reageEventoJogo (EventKey (SpecialKey KeyDown)   Down _ _) s | players s == 4 = s {jogadas = (jogadas s) {j4 = Just $ Movimenta B}}
                                                             | otherwise = s

reageEventoJogo (EventKey (SpecialKey KeyLeft)   Down _ _) s | players s == 4 = s {jogadas = (jogadas s) {j4 = Just $ Movimenta E}}
                                                             | otherwise = s

reageEventoJogo (EventKey (SpecialKey KeyRight)  Down _ _) s | players s == 4 = s {jogadas = (jogadas s) {j4 = Just $ Movimenta D}}
                                                             | otherwise = s

reageEventoJogo (EventKey (Char ',')  Down _ _) s | players s == 4 = s {jogadas = (jogadas s) {j4 = Just $ Dispara Canhao}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '.')  Down _ _) s | players s == 4 = s {jogadas = (jogadas s) {j4 = Just $ Dispara Laser}}
                                                  | otherwise = s

reageEventoJogo (EventKey (Char '-')  Down _ _) s | players s == 4 = s {jogadas = (jogadas s) {j4 = Just $ Dispara Choque}}
                                                  | otherwise = s

reageEventoJogo (EventKey (SpecialKey KeyTab)  Down _ _) s = s{posMenu = MenuInicial}
reageEventoJogo (EventKey (Char 'p')  Down _ _) s = s {posMenu = Pause}
reageEventoJogo _ s = s

-- ** Reagir ao passar do tempo
-- | Função que altera o 'State' do jogo com base no tempo.
reageTempo :: Float -> State -> State
reageTempo t s | posMenu s == Play && mode s == 1 =  menugameOver $ reageTempoBots t (s {estado = tick e3, jogadas=jogadasDefault})
               | posMenu s == Play && mode s == 0 = menugameOver $ s {estado = tick e3, jogadas=jogadasDefault}
               | posMenu s == Pause = s
               | otherwise = s
    where
        e = estado s
        moves = jogadas s
        e0 = maybejog 0 (j1 moves) e
        e1 = maybejog 1 (j2 moves) e0
        e2 = maybejog 2 (j3 moves) e1
        e3 = maybejog 3 (j4 moves) e2

-- | Função que recebe um estado e controla as 'Jogadas' dos bots.
reageTempoBots::Float -> State -> State
reageTempoBots _ s | players s == 1 = bot3
                   | players s == 2 = bot2
                   | players s == 3 = bot1
                   | otherwise = s
            where
                bot1 = s    {jogadas = (jogadas s   ){j4 = bot 4 (estado s)} }
                bot2 = bot1 {jogadas = (jogadas bot1){j3 = bot 3 (estado bot1)} }
                bot3 = bot2 {jogadas = (jogadas bot2){j2 = bot 2 (estado bot2)} }

-- | Função que determina se acabou o jogo e desenha o menu de fim de jogo.
menugameOver :: State -> State
menugameOver s | mode s == 1 && players s == 1 && (v1 == 0 || (v2 == 0 && v3 == 0 && v4 == 0)) = s {posMenu = GameOver}
               | mode s == 1 && players s == 2 && ((v1 == 0 && v2 == 0) || (v3 == 0 && v4 == 0)) = s {posMenu = GameOver}
               | mode s == 1 && players s == 3 && ((v1 == 0 && v2 == 0 && v3 == 0) || v4 == 0) = s {posMenu = GameOver}
               | mode s == 0 && players s == 2 && (v1 == 0 || v2 == 0) = s {posMenu = GameOver}
               | mode s == 0 && players s == 3 && (length (filter (==0) l3) == 2) = s {posMenu = GameOver}
               | mode s == 0 && players s == 4 && (length(filter (==0) l4) == 3) = s {posMenu = GameOver}
               | otherwise = s
               where
                v1 = vidasJogador $ head $ jogadoresEstado (estado s)
                v2 = vidasJogador $ jogadoresEstado (estado s) !! 1
                v3 = vidasJogador $ jogadoresEstado (estado s) !! 2
                v4 = vidasJogador $ jogadoresEstado (estado s) !! 3
                l3 = [v1,v2,v3]
                l4 = [v1,v2,v3,v4]


-- | Função que indica a jogada a efetuar pelo 'Jogador'.
maybejog :: Int -> Maybe Jogada -> Estado -> Estado
maybejog _ Nothing s = s
maybejog i (Just j) s = jogada i j s

-- ** Desenhar um 'State'
-- | Função responsável por desenhar o jogo.
desenhaEstado :: State -> Picture
desenhaEstado s | posMenu s == Play = desenhaMapa s
                | posMenu s == MenuInicial = scale f f $ menuInicial $ imagens s
                | posMenu s == MenuModeB = scale f f $ menuModeB $ imagens s
                | posMenu s == MenuModeP = scale f f $ menuModeP $ imagens s
                | posMenu s == MenuPlayer1 = scale f f $ menuPlayer1 $ imagens s
                | posMenu s == MenuPlayer2 = scale f f $ menuPlayer2 $ imagens s
                | posMenu s == MenuPlayer3 = scale f f $ menuPlayer3 $ imagens s
                | posMenu s == MenuPlayer4 = scale f f $ menuPlayer4 $ imagens s
                | posMenu s == MenuMapa1 = scale f f $ menuMapa1 $ imagens s
                | posMenu s == MenuMapa2 = scale f f $ menuMapa2 $ imagens s
                | posMenu s == MenuMapa3 = scale f f $ menuMapa3 $ imagens s
                | posMenu s == MenuMapa4 = scale f f $ menuMapa4 $ imagens s
                | posMenu s == Pause = Pictures [desenhaMapa s, scale f f $ pause $ imagens s]
                | posMenu s == GameOver = Pictures [desenhaMapa s, scale f f $ gameOver $ imagens s]
                | otherwise = Blank
                where
                    (wx, wy) = winSize s       
                    f = minimum [fromIntegral wx / fromIntegral tamanhoImagemMenu  ,  fromIntegral wy / fromIntegral tamanhoImagemMenu]

-- ** Desenhar 'Disparo's
-- | Função que desenha os 'DisparoCanhao' do 'State'.
desenhaDisparosC :: State -> Picture
desenhaDisparosC s = Pictures $ map (desenhaC img) d
                where
                 d = disparosEstado (estado s)
                 img = imagens s

-- | Função que desenha um 'DisparoCanhao'
desenhaC :: Imagens -> Disparo -> Picture
desenhaC imgs (DisparoCanhao _ (x,y) _ ) = Translate ox oy $ scale 0.5 0.5 img
            where
                img = canhao imgs
                oy = fromIntegral(-1*x*tamanhoImagemPeca) - fromIntegral tamanhoImagemPeca/2
                ox = fromIntegral(y*tamanhoImagemPeca) + fromIntegral tamanhoImagemPeca/2
desenhaC _ _ = Blank

-- | Função que desenha os 'DisparoChoque' do 'State'.
desenhaDisparosS :: State -> Picture
desenhaDisparosS s = Pictures $ map (desenhaS img j) d
                 where
                  j = jogadoresEstado (estado s)
                  d = disparosEstado (estado s)
                  img = imagens s

-- | Função que desenha um 'DisparoChoque'.
desenhaS :: Imagens -> [Jogador] -> Disparo -> Picture
desenhaS imgs j (DisparoChoque i _ ) = Translate ox oy $ scale 6 6 img
            where
                img = choque imgs
                (x,y) = posicaoJogador $ j !! i
                oy = fromIntegral(-1*x*tamanhoImagemPeca) - fromIntegral tamanhoImagemPeca/2
                ox = fromIntegral(y*tamanhoImagemPeca) + fromIntegral tamanhoImagemPeca/2
desenhaS _ _ _ = Blank


-- ** Desenhar 'Jogador'es
-- | Função que desenha os 'Jogador'es do 'State'.
desenhaJogadores:: State -> Picture
desenhaJogadores s = Pictures $ map (desenhaJ img) j
                where
                 j = jogadoresEstado (estado s)
                 img = imagens s

-- | Função que desenha um 'Jogador'.
desenhaJ :: Imagens -> Jogador -> Picture
desenhaJ _ (Jogador _ _ 0 _ _) = Blank
desenhaJ imgs (Jogador (x,y) d _ _ _) = Translate ox oy $ scale 2 2 img
    where
        img = rotateTank d $ tank imgs
        oy = fromIntegral(-1*x*tamanhoImagemPeca) - fromIntegral tamanhoImagemPeca/2
        ox = fromIntegral(y*tamanhoImagemPeca) + fromIntegral tamanhoImagemPeca/2

-- | Função que roda um tank conforme a sua direção.
rotateTank:: Direcao -> Picture -> Picture
rotateTank C img = img
rotateTank D img = Rotate 90 img
rotateTank B img = Rotate 180 img
rotateTank E img = Rotate 270 img 

-- | Função que determina o número total de jogadores conforme o modo de Jogo.
numeroplayers :: State -> State
numeroplayers s | mode s == 0 && players s == 2 = s{estado = Estado mE (take 2 jE) dE}
                | mode s == 0 && players s == 3 = s{estado = Estado mE (take 3 jE) dE}
                | otherwise =  s
                where
                   (Estado mE jE dE) = estado s

-- ** Desenhar 'Mapa'
-- | Função que recebe um 'State' e desenha o 'Mapa'.
desenhaMapa:: State -> Picture
desenhaMapa s = Translate (fromIntegral x/(-2) * 0.9) (fromIntegral y/2 * 0.9) $ scale (f*0.9) (f*0.9) $  Pictures[mapUnscaled, desenhaJogadores s, desenhaDisparosC s, desenhaDisparosS s]
            where
              mapa  = mapaEstado $ estado s
              mapUnscaled = pictures (desenhaLinhas (imagens s) mapa 0)
              f = scaleFactor s
              (x,y) = winSize s

-- | Função que recebe um 'State' e indica o fator de escala.
scaleFactor:: State -> Float
scaleFactor s = minimum [fromIntegral wx / fromIntegral tx, fromIntegral wy / fromIntegral ty]
    where
        (wx, wy) = winSize s
        (ty, tx) = (tamanhoImagemPeca * length (mapaEstado (estado s)), tamanhoImagemPeca * length (head ( mapaEstado (estado s))))

-- | Função que desenha as linhas do 'Mapa'.
desenhaLinhas:: Imagens -> Mapa -> Int -> [Picture]
desenhaLinhas _ [] _ = []
desenhaLinhas i (h:t) p = Translate 0 (fromIntegral p) (pictures(desenhaLinha i h 0)) : desenhaLinhas i t (p - tamanhoImagemPeca)

-- | Função que desenha um linha do 'Mapa'.
desenhaLinha:: Imagens -> [Peca] -> Int -> [Picture]
desenhaLinha _ [] _ = []
desenhaLinha i (h:t) p =  Translate (fromIntegral p) 0 (desenhaPeca h i) : desenhaLinha i t (p + tamanhoImagemPeca)

-- | Função que dada um 'Peca' devolve a sua respetiva 'Picture'.
desenhaPeca:: Peca -> Imagens -> Picture
desenhaPeca  Vazia                i = vazio i   
desenhaPeca (Bloco Indestrutivel) i = indestrutivel i
desenhaPeca (Bloco Destrutivel)   i = destrutivel i