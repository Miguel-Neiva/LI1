{-|
Module : Tarefa 5
Description : Módulo Haskell que executa uma jogada
Copyright : João Martins <a91669@alunos.uminho.pt>
            José Neiva <a92945@alunos.uminho.pt>
   O objetivo desta tarefa e definir uma função no qual, ao receber o estado do jogo, devolve um conjunto de jogadas, uma de cada fantasma,
    com a melhor alternativa que cada consegue para reagir ao Pacman.
   Existem duas formas alternativas de jogadas do ghost, quando este persegue ou quando foge.
   No caso de perseguir definimos uma função chase que através das coordenadas do Pacman vai analisar a melhor distancia até ao Pacman e caso
   não haja parede na jogada a seguir vai efetuar essa jogada, caso haja parede, de forma recursiva vai analisar a segunda melhor distância.
   No caso de o ghost estar vivo este vai perseguir o pacman, o pacman vai se mover sempre para a direita quando encontra uma parede.

-}
module Tarefa5 where 

import Types
import Data.List
import Tarefa2


-- | Função que devolve listas com as distancias e respetivas orientações para cada orientação
mudaestadochase :: Player -- ^recebe um Ghoststate
                -> Player -- ^recebe um Pacstate
                ->[(Int,Orientation)]  -- ^retorna uma lista com as distancias e respetivas orientações
mudaestadochase (Ghost(GhoState (a,(colGhos,linhaGhos),velocidade, e, f, g) i)) (Pacman (PacState (j,(colPac,linhaPac),m,orient, pontos, q) r s t)) = sortOn fst ([(abs((colGhos+1)-(colPac))+abs(linhaGhos-linhaPac),D),(abs((colGhos-1)-(colPac))+abs(linhaGhos-linhaPac),U),(abs(colGhos-colPac)+abs((linhaGhos+1)-linhaPac),R),(abs(colGhos-colPac)+abs((linhaGhos-1)-linhaPac),L)]) 
    

-- | função que analisa o menor e verifica se está  ou não numa parede
mudaorientacaochase :: Maze -- ^recebe um maze
                    -> Player -- ^recebe um ghostate
                    -> [(Int,Orientation)] -- ^ recebe uma lista das distâncias e respetivas orient provenientas da função mudaestadochase
                    -> Orientation -- ^muda a orientação analisando a melhor distancia sem parede(recursivamente)
mudaorientacaochase maze abrev@(Ghost (GhoState (a,(colGhos,linhaGhos),velocidade, orient, f, g) i)) ((dist,b):t) = case b of 
                                                                                                  R -> if (procuracorridor (procuramaze maze colGhos) (linhaGhos+1)) /= Wall then R else mudaorientacaochase maze abrev t
                                                                                                  L -> if (procuracorridor (procuramaze maze colGhos) (linhaGhos-1)) /= Wall then L else mudaorientacaochase maze abrev t
                                                                                                  U -> if (procuracorridor (procuramaze maze (colGhos-1)) (linhaGhos)) /= Wall then U else mudaorientacaochase maze abrev t
                                                                                                  D -> if (procuracorridor (procuramaze maze (colGhos+1)) (linhaGhos)) /= Wall then D  else mudaorientacaochase maze abrev t
-- | função retorna uma jogada com a melhor orientaçao possivel
chaseMode :: State -- ^recebe um estado de jogo
          -> Int -- ^recebe o id do Player
          -> Play -- ^retorna o estado com a jogada a seguir
chaseMode (State maze l nvl) x = Move x orientacaochase



 where pac = identificaPac l
       ghostmovivel = identificaplayer x l 
       orientacaochase = mudaorientacaochase maze ghostmovivel (mudaestadochase ghostmovivel pac)

-- | Função para identificar o Pacman
identificaPac :: [Player] -- ^recebe a lista dos /state/
              -> Player -- etorna o pacman da lista
identificaPac ((Pacman x):t) = Pacman x
identificaPac (h:t) = identificaPac t 

-- | função que muda a orientação do ghost quando está a fugir sempre para a direita quando encontra uma parede
mudaorientacaoscatter :: Maze -- ^recebe um labirinto
                      -> Player -- ^recebe um ghostate
                      -> Orientation -- ^retorna a orientação a seguir
mudaorientacaoscatter maze (Ghost (GhoState (a,(colGhos,linhaGhos),velocidade, orient, f, g) i)) 
    |(procuracorridor (procuramaze maze colGhos) (linhaGhos-1)) == Wall && orient==L  = U
    |(procuracorridor (procuramaze maze colGhos) (linhaGhos+1)) == Wall && orient==R  = D
    |(procuracorridor (procuramaze maze (colGhos-1)) (linhaGhos)) == Wall && orient==U  = R
    |(procuracorridor (procuramaze maze (colGhos+1)) (linhaGhos)) == Wall && orient==D  = L
    |otherwise = R

-- | função que retorna a jogada quando o ghost está a fugir
scattermode :: State -- ^recebe um estado
            -> Int -- ^recebe um id
            -> Play -- ^retorna uma jogada com a respetiva orientação
scattermode (State maze l nvl) x = Move x orientacaoscatter
    
    where orientacaoscatter = mudaorientacaoscatter maze (identificaplayer x l)

-- | função que analisa o modo do ghost e manda para a respetiva função
getPlay :: Player -- ^recebe um ghostate
        -> State -- ^recebe um estado
        -> Play -- ^ retorna a jogada com a orientação respetiva
getPlay (Ghost (GhoState (a,(colGhos,linhaGhos),velocidade, orient, f, g) i)) state
    |i== Alive = chaseMode state a
    |i== Dead = scattermode state a


-- | Funcao que encontra o proximo fantasma a ser jogado e um inteiro para o identificar o caso de a fila acabar e tambem de ser o pacman
nextghost :: Int -- ^recebe um acumulador
          -> [Player] -- ^recebe a lista de jogadores do /state/
          -> (Player,Int) -- ^devolve o jogador e um inteiro que nos da a informacao se a lista acabou ou nao ou se e pacman
nextghost _ [] = (Ghost (GhoState (-1,(-1,-1),-1, Null, -1, -1) Alive),1)
nextghost 0 (ghost@(Ghost (GhoState (a,(colGhos,linhaGhos),velocidade, orient, f, g) i)):t) = (ghost,0)
nextghost 0 (h:t) = (h,2)
nextghost i (h:t) = nextghost (i-1) t


-- | Funcao auxiliar que fazem a lista de play
ghostPlayAux :: Int -- ^recebe um acumulador
             -> State -- ^recebe um state
             -> [Play] -- ^retorna a lista de plays
ghostPlayAux i estado@(State m l n)
 |jaacabou == 2 = ghostPlayAux (i+1) estado
 |jaacabou == 1 = []
 |otherwise = play:(ghostPlayAux (i+1) estado)
    where  (ghost,jaacabou) = nextghost i l
           play = getPlay ghost estado

-- |Funcao principal que faz as plays
ghostPlay :: State -- ^recebe um estado
          -> [Play] -- ^retorna uma lista de jogadas
ghostPlay estado = ghostPlayAux 0 estado 
          
