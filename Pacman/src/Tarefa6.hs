{-|
Module : Tarefa 4
Description : Módulo Haskell que faz o Bot
Copyright : João Martins <a91669@alunos.uminho.pt>
            José Neiva <a92945@alunos.uminho.pt>
O objetivo desta tarefa é calcular a melhor jogada para o Pacman, para isto, nos calculamos o ghost mais perto do pacman
e dependendo do do seu modo,Normal ou Mega, fugia dele ou perseguia-o
-}
module Tarefa6 where 

import Tarefa2
import Types
import Data.List

-- | Calcula a distancia do pac a cada ghost dos players e faz uma lista com a respectiva distancia e identificador
closestGhost :: Player -- ^recebe o pacman
             -> [Player] -- ^recebe a lista de jogadores
             -> [(Int,Int)] -- ^retorna uma lista com o identificador e a distancia do pacman ao respetivo ghost
closestGhost _ [] = []
closestGhost pac@(Pacman (PacState (identificadorP,(xp,yp),speed, o,p, v) time mouth mode)) (h:t) = case h of 
                                                                                                    (Ghost (GhoState (identificadorG,(xg,yg),speed, o, p,v) mode)) -> (abs(xp-xg)+abs(yp-yg),identificadorG):closestGhost pac t
                                                                                                    otherwise -> closestGhost pac t

-- | Funcao que calcula a melhor jogada para o pacman
moveBot :: Maze -- ^recebe o labirinto
        -> Player -- ^recebe o pacman
        -> Player -- ^recebe o ghost mais perto do pacman
        -> Orientation -- ^ retorna a melhor orientacao para o pacman tomar
moveBot maze (Pacman (PacState (identificadorP,(xp,yp),speedP,op,pP,vP) time mouth Mega)) (Ghost (GhoState (identificadorG,(xg,yg),speedG, og,pG,vG) modeG)) 
 |(xg == xp && yp<yg) || (abs(xp-xg) < abs(yp-yg) && yp<yg) = if (procuracorridor (procuramaze maze xp) (yp+1)) /= Wall then R else if (procuracorridor (procuramaze maze (xp+1)) (yp)) /= Wall then D else if (procuracorridor (procuramaze maze (xp-1)) (yp)) /= Wall then U else L
 |(xg == xp && yp>yg) || (abs(xp-xg) < abs(yp-yg) && yp>yg) = if (procuracorridor (procuramaze maze xp) (yp-1)) /= Wall then L else if (procuracorridor (procuramaze maze (xp+1)) (yp)) /= Wall then D else if (procuracorridor (procuramaze maze (xp-1)) (yp)) /= Wall then U else R
 |(yp == yg && xp<xg) || (abs(xp-xg) > abs(yp-yg) && xp<xg) = if (procuracorridor (procuramaze maze (xp+1)) (yp)) /= Wall then D else if (procuracorridor (procuramaze maze xp) (yp+1)) /= Wall then R else if (procuracorridor (procuramaze maze xp) (yp-1)) /= Wall then L else U
 |(yp == yg && xp>xg) || (abs(xp-xg) > abs(yp-yg) && xp>xg) = if (procuracorridor (procuramaze maze (xp-1)) (yp)) /= Wall then U else if (procuracorridor (procuramaze maze xp) (yp+1)) /= Wall then R else if (procuracorridor (procuramaze maze xp) (yp-1)) /= Wall then L else D
 |otherwise = op
moveBot maze (Pacman (PacState (identificadorP,(xp,yp),speedP,op,pP,vP) time mouth Normal)) (Ghost (GhoState (identificadorG,(xg,yg),speedG, og,pG,vG) modeG))
 |(xg == xp && yp<yg) || (abs(xp-xg) < abs(yp-yg) && yp<yg) = if (procuracorridor (procuramaze maze xp) (yp-1)) /= Wall then L else if (procuracorridor (procuramaze maze (xp+1)) (yp)) /= Wall then D else if (procuracorridor (procuramaze maze (xp-1)) (yp)) /= Wall then U else R
 |(xg == xp && yp>yg) || (abs(xp-xg) < abs(yp-yg) && yp>yg) = if (procuracorridor (procuramaze maze xp) (yp+1)) /= Wall then R else if (procuracorridor (procuramaze maze (xp+1)) (yp)) /= Wall then D else if (procuracorridor (procuramaze maze (xp-1)) (yp)) /= Wall then U else L
 |(yp == yg && xp<xg) || (abs(xp-xg) > abs(yp-yg) && xp<xg) = if (procuracorridor (procuramaze maze (xp-1)) (yp)) /= Wall then U else if (procuracorridor (procuramaze maze xp) (yp+1)) /= Wall then R else if (procuracorridor (procuramaze maze xp) (yp-1)) /= Wall then L else D
 |(yp == yg && xp>xg) || (abs(xp-xg) > abs(yp-yg) && xp>xg) = if (procuracorridor (procuramaze maze (xp+1)) (yp)) /= Wall then D else if (procuracorridor (procuramaze maze xp) (yp+1)) /= Wall then R else if (procuracorridor (procuramaze maze xp) (yp-1)) /= Wall then L else U
 |otherwise = op






-- | Bot calcula uma lista com distancia dos ghost e ordenada do mais perto para o mais longe depois calcula a melhor jogada para o respectivo modo de pac
bot :: Int -- ^recebe o identificador do pacman
    -> State  -- ^recebe um estado
    -> Maybe Play -- ^retorna a melhor orientacao que o pacman pode tomar
bot i (State m l n) = Just (Move i move)
 where pac@(Pacman (PacState (identificador,(x,y),speed, o,p, v) time mouth mode)) = identificaplayer i l
       listadist@((d,idG):t) = sortOn fst (closestGhost pac l)
       ghost = identificaplayer idG l 
       move = moveBot m pac ghost
       
