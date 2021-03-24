{-|
Module : Tarefa 4
Description : Módulo Haskell que faz o jogo agir com o tempo
Copyright : João Martins <a91669@alunos.uminho.pt>
            José Neiva <a92945@alunos.uminho.pt>
O objetivo desta tarefa é fazer com que o tempo seja um fator no nosso jogo. 
Por isto tivemos que ter em conta os seguintes fatores:
-Fazer com que a boca do pac mude que foi resolvido com uma simples funcao que procurava o pacman na lista e mudava a boca 
-Com que o "tempo Mega" decresca e o estado do fantasma reagisse a isso
-Executar as jogadas que foram fabricadas na tarefa 5 e tambem as do pacman
-}
module Tarefa4 where 
import Tarefa2
import Tarefa5
import Types


defaultDelayTime = 250 -- 250 ms


-- | Funcao que muda a boca a cada iteracao
mudaboca :: [Player] -- ^recebe uma lista de jogadores
         -> [Player] -- ^devolve a lista de jogadores com a boca mudada
mudaboca (h:t) = case h of (Pacman (PacState (identificador,(x,y),speed, o,p, v) time Open mode)) -> (Pacman (PacState (identificador,(x,y),speed, o,p, v) time Closed mode)):t
                           (Pacman (PacState (identificador,(x,y),speed, o,p, v) time Closed mode)) -> (Pacman (PacState (identificador,(x,y),speed, o,p, v) time Open mode)):t
                           otherwise -> h:mudaboca t

-- | Funcao que faz os ghost voltarem ao Alive quando o timemega acabar
voltaNormal :: [Player] -- ^recebe uma lista de jogadores
            -> [Player] 
voltaNormal [] = []
voltaNormal ((Ghost (GhoState (identificador,(x,y),speed, o, p,v) mode)):t) = (Ghost (GhoState (identificador,(x,y),speed*2, o, p,v) Alive)):voltaNormal t;
voltaNormal (h:t) = h:voltaNormal t;


-- | So acontece quando player esta em mega e retorna um par com a lista e com um bool que representa se o mega acabou ou nao
baixaTempo :: [Player] -- ^recebe uma lista de jogadores
            -> ([Player],Bool) -- ^retorna a lista com time-1 e um bool que identifica se o time mega chegou a 0
baixaTempo (h:t) = case h of (Pacman (PacState (identificador,(x,y),speed, o,p, v) time mouth mode)) -> if (time-1) <= 0 then ((Pacman (PacState (identificador,(x,y),speed, o,p, v) 0 mouth Normal)):t,True) else ((Pacman (PacState (identificador,(x,y),speed, o,p, v) (time-1) mouth mode)):t,False)
                             otherwise ->(h:t1,b);  
 where (t1,b) = baixaTempo t


-- | Funcao que percorre a lista até encontrar o pacman e depois envia a orientacao e a sua identicacao para executar a play do pacman

getOrientation :: [Player] -- ^recebe uma lista de jogadores
                -> (Orientation,Int) -- ^retorna um par (orentacao,identificador) a orientacao e a que o identificador vai tomar
getOrientation ((Pacman (PacState (identificador,(x,y),speed, o,p, v) time mouth mode)):t) = (o,identificador) 
getOrientation ((Ghost (GhoState (identificador,(x,y),speed, o, p,v) mode)):t) = getOrientation t



-- | Funcão que executa a jogado do Pacman para a orientacao que ele se encontra
pacMove ::State -- ^recebe um estado
        -> State -- ^retorna um estado com a jogada do pacman
pacMove estado@(State m l n) = (play (Move identificador orientacao) estado)
 where (orientacao,identificador) = getOrientation l



-- | Uma funcao que muda a orientacao para podermos usar na funcao na funcao do main /updateControlledPlayer/
changedirection :: Orientation -- ^recebe uma orientacao
                -> [Player] -- ^recebe uma lista de jogadores
                -> [Player] -- ^retorna a lista com a orientacao do pacman mudada
changedirection _ [] = []
changedirection orientacao ((Pacman (PacState (identificador,(x,y),speed, o,p, v) time Open mode)):t) = ((Pacman (PacState (identificador,(x,y),speed, orientacao,p, v) time Open mode)):t)
changedirection orientacao (h:t) = h:changedirection orientacao t

-- | Uma funcao que executa todas as plays calculadas na tarefa 5 analisando tambem o step
applyghostplays :: Int -- ^recebe o step
                -> State -- ^recebe um estado
                -> [Play] -- ^recebe uma lista de jogadores
                -> State -- ^retorna o state com as jogadas de todos os ghosts
applyghostplays _ state [] = state
applyghostplays  step state@(State m l n) (h@(Move i o):t) = if (mod step 2) /= 0 || v==1 then (play h statedone) else statedone
 where statedone = applyghostplays step state t
       Ghost (GhoState (identificador,(x,y),speed, o, p,v) mode) = identificaplayer i l

-- |Funcao principal que faz o tempo passar, verifica se ha time mega e envia os dados para as funcoes necessarias
passTime :: Int -- ^recebe um step
         -> State -- ^recebe um estado
         -> State -- ^retorna um estado com todas as alteracoes
passTime x estado@(State m l n) 
 |b==True = pacMove (applyghostplays x (State m (mudaboca (voltaNormal l1)) n) ghostPlays)
 |otherwise = pacMove (applyghostplays x (State m (mudaboca l1) n) ghostPlays)
  where (l1,b) = baixaTempo l                  
        ghostPlays = ghostPlay estado


