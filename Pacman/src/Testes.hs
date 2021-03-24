module Testes where
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6
import Types

listatunel ::[Player]
listatunel = [(Pacman (PacState (0,(10,19),1,R,4,1) 0 Open Normal))]

listamega :: [Player]
listamega = [(Pacman (PacState (0,(1,2),1,R,4,1) 0 Open Mega)),Ghost (GhoState (1,(1,3),1,L,10,20) Dead)]

listatestezinh :: [Player]
listatestezinh = [(Ghost (GhoState (1,(28,14),1,L,10,20) Alive)),(Pacman (PacState (0,(28,13),1,R,4,1) 0 Open Normal))]


listaplayers :: [Player]
listaplayers = [(Pacman (PacState (0,(1,1),1,R,4,1) 0 Open Normal)),Ghost (GhoState (2,(1,2),1,L,10,20) Alive),Ghost (GhoState (3,(7,16),1,L,10,20) Alive)]

listaparede :: [Player]
listaparede = [(Pacman (PacState (0,(4,1),1,L,4,1) 0 Open Normal)),Ghost (GhoState (1,(5,1),1,L,10,20) Alive)]

estadotest :: State
estadotest = (State (generateMaze 20 21 1369411474) listaplayers 1)


listaandar2 :: [Player]
listaandar2 = [(Pacman (PacState (0,(1,1),1,R,4,1) 0 Open Normal))]
testesaux :: String
testesaux = "\nTarefa1\n" ++ "Mapa com 2 valores pares (20,20)\n" ++ mazetostring(generateMaze 20 20 111111)
            ++ "\nMapa com 2 valores impares (21,21)\n" ++ mazetostring(generateMaze 21 21 111111)
            ++ "\nMapa com altura par e comprimento impar (20,21)\n" ++ mazetostring(generateMaze 20 21 11111)
            ++ "\nMapa com altura impar e comprimento par (21,20)\n" ++ mazetostring(generateMaze 21 20 11111)
            ++ "\nTarefa 2\n"
            ++ "testes de orientação\n" ++ "Pacman\npara os lados\n" ++ statetostring (play (Move 1 R) (play (Move 0 L) (State (generateMaze 20 21 1231111) listaplayers 1))) ++  "\nLista original\n" ++ show listaplayers ++ "\n"
            ++ "\npara Up\n" ++  statetostring (play (Move 0 U) (play (Move 1 U) (State (generateMaze 20 21 1369411474) listaplayers 1))) ++  "\nLista original\n" ++ show listaplayers ++ "\n"
            ++ "\npara baixo\n" ++  statetostring (play (Move 0 D) (play (Move 1 D) (State (generateMaze 20 21 1369411474) listaplayers 1))) ++  "\nLista original\n" ++ show listaplayers ++ "\n"
            ++ "\nGhost\npara os lados\n" ++  statetostring (play (Move 2 L) (play (Move 3 R) (State (generateMaze 20 21 1369411474) listaplayers 1))) ++  "\nLista original\n" ++ show listaplayers ++ "\n"
            ++ "\nPara UP\n" ++  statetostring (play (Move 2 U) (play (Move 3 U) (State (generateMaze 20 21 1369411474) listaplayers 1))) ++  "\nLista original\n" ++ show listaplayers ++ "\n"
            ++ "\npara baixo\n" ++  statetostring (play (Move 2 D) (play (Move 3 D) (State (generateMaze 20 21 1369411474) listaplayers 1)))
            ++ "\n\nLista original\n" ++ show listaplayers ++ "\n"
            ++ "testes de mudanca de mapa\n" ++ statetostring (play (Move 1 L) (play (Move 0 R) (State (generateMaze 20 21 1369411474) listaplayers 1))) ++  "\nLista original\n" ++ show listaplayers ++ "\n"
            ++ "\npara quando bater na parede\n" ++ statetostring (play (Move 0 L) (play (Move 1 L) (State (generateMaze 20 21 1369411474) listaparede 1))) ++  "\nLista original\n" ++ show listaparede ++ "\n"
            ++ "\npara quando o ghost bate no pacman\n" ++ statetostring ((play (Move 2 L) (State (generateMaze 20 21 1369411474) listaplayers 1))) ++  "\nLista original\n" ++ show listaplayers
            ++ "\nTunel\n" ++ statetostring (play (Move 0 R)(play (Move 0 R) (State (generateMaze 20 21 1369411474) listatunel 1))) ++  "\nLista original\n" ++ show listatunel ++ "\n"
            ++ "\ntelepore para o meio e pontuacao\n" ++ statetostring ((play (Move 0 R) (State (generateMaze 31 30 1369411474) listamega 1))) ++ "\n lista original\n" ++ show listamega ++ "\n"
            ++ "\nTunel ir e vir\n" ++ statetostring (play (Move 0 L) (play (Move 0 L) (play (Move 0 R) (State (generateMaze 20 21 1369411474) listatunel 1)))) ++  "\nLista original\n" ++ show listatunel ++ "\n"
           ++ "\ncome big food e come ghost\n" ++ statetostring ((play (Move 0 R) (State (generateMaze 31 30 1369411474) listatestezinh 1))) ++ "\n lista original\n" ++ show listatestezinh ++ "\n"
statetostring :: State -> String
statetostring (State m l i) = mazetostring m ++ "\n" ++ show l ++ " Nivel:" ++ show i

testes :: IO()
testes = putStr(testesaux)

