{-|
Module : Tarefa 2
Description : Módulo Haskell que executa uma jogada
Copyright : João Martins <a91669@alunos.uminho.pt>
            José Neiva <a92945@alunos.uminho.pt>
O objetivo deste módulo é executar uma jogada de um pacman ou de um fantasma atraves do numero de identidade deste e de uma orientação.
No caso do Pacman temos que verifica a peca para qual o pacman se move fazendo as respectivas mudancas no seu estado, na pontuação e na sua vida.
o estado muda para mega no caso do Pacman comer uma big food que tambem provoca uma mudanca de estado para todos os fantasmas do jogo para Dead
o estado muda para dying no caso do pacman ter 0 vidas e bater contra um fantasma no estado alive se isto acontecer mas o pacman tem mais vidas é lhe retirado uma
a pontuacao muda em 3 casos:
-come little food é lhe fornecido +1 pontos
-come big food é lhe fornecido +5 pontos
-come um fantasma em estado dying é lhe fornecido +10 pontos
-}
module Tarefa2 where

import Types

--funcoes procura

-- |Função que procura num Maze um corredor que esta no indice y
procuramaze :: Maze -- ^ recebe um labirinto
 -> Int -- ^ recebe um indice
 -> Corridor -- ^ retorna um corredor
procuramaze _ (-1) = [Empty]
procuramaze (h:t) 0 = h
procuramaze (h:t) y = procuramaze t (y-1)

-- | Procura uma peça que esta no indice x
procuracorridor :: Corridor -- ^ recebe um corredor
 -> Int -- ^ recebe um indice
 -> Piece -- ^retorna a peca que esta nesse indice
procuracorridor _ (-1) = Empty
procuracorridor (h:t) 0 = h
procuracorridor l@(h:t) x
 |x<0 || x==length l = Empty
 |otherwise = procuracorridor t (x-1)

-- funcoes que substituem o mapa

-- | Função similar a @procuracorridor@ que procura um indice de um corredor e substitui o seu conteudo por Empty pois o pacman comeu a peça
substituicorredor :: Int -- ^recebe um indice
 -> Corridor -- ^recebe um corredor
 -> Corridor -- ^retorna um corredor modificado
substituicorredor 0 (h:t) = (Empty:t)
substituicorredor x (h:t) = h:(substituicorredor (x-1) t)

-- |Funçao similar a @procuramaze@ que atraves de coordenadas procura o respectivo corredor e depois envia para a função @substituicorredor@ para este modificar a a peca no caso (-1,_) acontece quando a peca nao precisa de ser substituida
substituipeca :: Coords -- ^recebe duas coordenadas
 -> Maze -- ^recebe um labirinto a ser modificado
 -> Maze -- ^retorna um labirinto modificado
substituipeca (_,-1) m = m
substituipeca (0,y) (h:t) = (substituicorredor y h):t
substituipeca (x,y) m@(h:t) = if(y==length h) then m else h:(substituipeca (x-1,y) t)



-- |Função que faz as respectivas mudancas no estado,pontuação e velocidade consoante a peca dada
estadojogador :: Piece -- ^ recebe uma peça
 -> [Player] -- ^ recebe uma lista de Players
 -> [Player] -- ^ retorna uma lista de players com as respectivas mudancas
estadojogador Empty l = l
estadojogador Wall l = l
estadojogador _ [] = []
estadojogador x ((Pacman (PacState (a,(b,c),d, e, pontos, g) h i j)):t) = case x of
                                                                       Food Little -> Pacman (PacState (a,(b,c),d, e, pontos+1, g) h i j):t1
                                                                       Food Big -> Pacman (PacState (a,(b,c),d, e, pontos+5, g) 10 i Mega):t1
                                                                       
 where t1 = estadojogador x t
estadojogador x ((Ghost (GhoState (a,(b,c),velocidade, e, f, g) i)):t) = case x of
                                                                    Food Big -> Ghost (GhoState (a,(b,c),velocidade/2, e, f, g) Dead):t1
                                                                    otherwise -> Ghost (GhoState (a,(b,c),velocidade, e, f, g) i):t1
 where t1 = estadojogador x t

-- | Identifica um player atraves do seu ID
identificaplayer :: Int -- ^ recebe um valor identificador
 -> [Player] -- ^ recebe uma lista
 -> Player -- ^ retorna o jogador que corresponde ao inteiro
identificaplayer x (h:t) = case h of
                                (Pacman (PacState (identificador,coords,d, e, f, g) time i j)) -> if x==identificador then h else identificaplayer x t
                                (Ghost (GhoState (identificador,coords,k, l, m, n) o)) -> if x==identificador then h else identificaplayer x t


-- | Função que muda o estado do ghost e de pac se tiveram uma colisao o caso (-1,-1) é quando não existe colisao e o caso (p,-1) e quando apenas se modifica o pacman
mudadead :: Int -- ^recebe o comprimento do maze
 -> Int -- ^ recebe a altura do maze
 -> (Int,Int) -- ^ recebe dois ID o primeiro do pacman e o segundo do ghost a ser modificado
 -> [Player] -- ^ recebe uma lista de jogadores
 -> [Player] -- ^ retorna uma lista de jogadores com o par de pac e ghost modificado
mudadead _ _ _ [] = []
mudadead _ _ (-1,-1) l = l
mudadead x y (p,-1) (h:t) = case h of
                                (pac@(Pacman (PacState (a,(b,c),d, e, f, vidas) h i j))) -> if p==a then if vidas==0 then ((Pacman (PacState (a,(b,c),d, e, f, vidas) h i Dying)):t) else ((Pacman (PacState (a,(b,c),d, e, f,vidas-1) h i j)):t) else pac:mudadead x y (p,-1) t
                                otherwise -> h:mudadead x y (p,-1) t
mudadead x y (p,g) (h:t) = case h of
                              (pac@(Pacman (PacState (a,(b,c),d, e, pontos, vidas) h i j))) -> if p==a then (Pacman (PacState (a,(b,c),d, e, pontos+10, vidas) h i j)):mudadead x y (p,g) t else pac:mudadead x y (p,g) t
                              (ghost@(Ghost (GhoState (identificador,(b,c),k, l, m, n) j))) -> if g==identificador then (Ghost (GhoState (identificador,((div x 2),(div y 2)-1),k, l, m, n) Alive)):mudadead x y (p,g) t else ghost:mudadead x y (p,g) t


-- |Verifica se o jogador que se moveu entra em colisao com algum outro jogador se não houve colisao é retornado (-1,-1) e se for apenas preciso modificar o pacman e retornado (pacman,-1)
verificacolisao :: Player
 -> [Player] -- ^ recebe uma lista de jogador
 -> (Int,Int) -- ^ retorna um par de ID (pacman,ghost) que teve em colisao
verificacolisao _ [] = (-1,-1)
verificacolisao pac@(Pacman (PacState (a,(b,c),d, e, f, g) time i j)) (h:t) = case h of
                                                                                (Ghost (GhoState (identificador,(x,y),k, l, m, n) Alive)) -> if x==b && c==y then (a,-1) else verificacolisao pac t
                                                                                (Ghost (GhoState (identificador,(x,y),k, l, m, n) Dead)) -> if x==b && c==y then (a,identificador) else verificacolisao pac t
                                                                                otherwise -> verificacolisao pac t
verificacolisao ghost@(Ghost (GhoState (identificador,(x,y),k, l, m, n) estado)) (h:t) = case h of
                                                                                          (Pacman (PacState (a,(b,c),d, e, f, g) h i Mega)) -> if x==b && c==y then (a,identificador) else verificacolisao ghost t
                                                                                          (Pacman (PacState (a,(b,c),d, e, f, g) h i j)) -> if x==b && c==y then (a,-1)  else verificacolisao ghost t
                                                                                          otherwise -> verificacolisao ghost t


-- | vai ha lista de players identifica qual o player por o identificador faz os testes da parede e do tunel depois modifica as coordenadas do jogador e retorna tambem essas mesmas coordenadas para modificar o labirinto depois
playPlayer :: Maze -- recebe o labirinto
 -> Play -- ^ recebe a jogada
 -> [Player] -- ^ recebe a lista de jogadores
 -> ([Player],Coords) -- ^ retorna a lista modificada e a coordenada modificada
--para pacman
playPlayer maze m@(Move x z) ((Pacman (PacState (a,(b,c),d, e, f, g) h i j)):t)
 |x==a = if z/=e then (((Pacman (PacState (a,(b,c),d, z, f, g) h i j)):t),n) else case z of R -> if (procuracorridor (procuramaze maze b) (c+1)) /= Wall then if tamanho/= (c+1) then (((Pacman (PacState (a,(b,c+1),d, z, f, g) h i j)):t),(b,c+1)) else (((Pacman (PacState (a,(b,0),d, z, f, g) h i j)):t),(b,0))  else (((Pacman (PacState (a,(b,c),d, z, f, g) h i j)):t),(b,c))
                                                                                            L -> if (procuracorridor (procuramaze maze b) (c-1)) /= Wall then if (-1)/= (c-1) then (((Pacman (PacState (a,(b,c-1),d, z, f, g) h i j)):t),(b,c-1)) else (((Pacman (PacState (a,(b,tamanho-1),d, z, f, g) h i j)):t),(b,tamanho-1)) else (((Pacman (PacState (a,(b,c),d, z, f, g) h i j)):t),(b,c))
                                                                                            U -> if (procuracorridor (procuramaze maze (b-1)) c) /= Wall then (((Pacman (PacState (a,(b-1,c),d, z, f, g) h i j)):t),(b-1,c)) else (((Pacman (PacState (a,(b,c),d, z, f, g) h i j)):t),(b,c))
                                                                                            D -> if (procuracorridor (procuramaze maze (b+1)) c) /= Wall then (((Pacman (PacState (a,(b+1,c),d, z, f, g) h i j)):t),(b+1,c)) else (((Pacman (PacState (a,(b,c),d, z, f, g) h i j)):t),(b,c))
 |otherwise = ((Pacman (PacState (a,(b,c),d, e, f, g) h i j)):t1,x2)
  where n = (-1,-1)
        (t1,x2) = (playPlayer maze m t)
        tamanho = length (head maze)
 --para fantasma
playPlayer maze m@(Move x z) ((Ghost (GhoState (a,(b,c),d, e, f, g) i)):t)
 |x==a = if z/=e then (((Ghost (GhoState (a,(b,c),d, z, f, g) i)):t),n) else case z of R -> if (procuracorridor (procuramaze maze b) (c+1)) /= Wall then if tamanho/= (c+1) then (((Ghost (GhoState (a,(b,c+1),d, z, f, g) i)):t),n) else (((Ghost (GhoState (a,(b,0),d, z, f, g) i)):t),n) else (((Ghost (GhoState (a,(b,c),d, z, f, g) i)):t),n)
                                                                                       L -> if (procuracorridor (procuramaze maze b) (c-1)) /= Wall then if (-1)/= (c-1) then (((Ghost (GhoState (a,(b,c-1),d, z, f, g) i)):t),n) else (((Ghost (GhoState (a,(b,tamanho),d, z, f, g) i)):t),n) else (((Ghost (GhoState (a,(b,c),d, z, f, g) i)):t),n)
                                                                                       U -> if (procuracorridor (procuramaze maze (b-1)) c) /= Wall then (((Ghost (GhoState (a,(b-1,c),d, z, f, g) i)):t),n) else (((Ghost (GhoState (a,(b,c),d, z, f, g) i)):t),n)
                                                                                       D -> if (procuracorridor (procuramaze maze (b+1)) c) /= Wall then (((Ghost (GhoState (a,(b+1,c),d, z, f, g) i)):t),n) else (((Ghost (GhoState (a,(b,c),d, z, f, g) i)):t),n)
 |otherwise = (((Ghost (GhoState (a,(b,c),d, e, f, g) i)):t1),x2)
  where n = (-1,-1)
        (t1,x2) = (playPlayer maze m t)
        tamanho = length (head maze)



-- | Funçao /mãe/ que junta tudo e manda para as funções que modificam o mapa e tambem a lista de jogadores
play :: Play -- ^ recebe uma jogada
 -> State -- ^ recebe um estado
 -> State -- ^ retorna o estado modificado por a jogada
play p@(Move identificador orientação) (State m l i)  = (State (substituipeca c m) listafinal i)
  where (l1,c@(x,y)) = playPlayer m p l
        playerfinal = estadojogador(procuracorridor (procuramaze m x) y) l1
        altura = length m
        comprimento = length (head m)
        listafinal = (mudadead altura comprimento (verificacolisao (identificaplayer identificador playerfinal) playerfinal) playerfinal)
