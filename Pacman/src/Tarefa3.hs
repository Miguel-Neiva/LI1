{-|
Module : Tarefa 3
Description : Módulo Haskell que compacta o mapa
Copyright : João Martins <a91669@alunos.uminho.pt>
            José Neiva <a92945@alunos.uminho.pt>
O objetivo deste módulo e compactar o mapa em instruções que são uma lista de instruct que podem ser um conjunto [(Int,Piece)]
que o int representa o numero de vezes que essa peça aparece no corredor de seguida ou repeat Int que so acontece quando a mesma instruct ja
aconteceu anteriormente onde o Int representa o indice na lista.
Utilizei par [(Int,Piece)] em vez de instruct na maior parte das funções para facilitar os testes.
-}
module Tarefa3 where

import Types

-- |Função que pega num corredor e tranforma de maneira simples para (1,piece)
compactcorridoraux :: Corridor -- ^ recebe um corridor
 -> [(Int,Piece)] -- ^ dá uma lista de pares (int,Piece)
compactcorridoraux [] = []
compactcorridoraux (h:t) = ((1,h):compactcorridoraux t)

-- |Função que junta as peças seguidas como esta proposto na Tarefa
juntaseguidos :: [(Int,Piece)] -- ^ recebe uma lista de pares
 -> [(Int,Piece)] -- ^ dá uma lista de pares ordenados sem peças seguidas repetidas
juntaseguidos (h:[]) = [h]
juntaseguidos ((a1,p1):(a2,p2):t)
  |p1==p2 = juntaseguidos ((a1+a2,p1):t)
  |otherwise = (a1,p1):juntaseguidos ((a2,p2):t)

-- | Funçao que verifica se há corredores iguais para substituir os a seguir por Repeat int e usa um acumulador para sabermos qual e o indicie para usarmos na @compactVerticalAux@
compactVertical :: Int -- ^ recebe um inteiro que acumula até a instrução ser igual
 -> Instructions -- ^ recebe as instruçoes
 -> Instructions -- ^ dá uma lista de instruçoes com os respetivos Repeats
compactVertical x [] = []
compactVertical _ (h:[]) = [h]
compactVertical x (h:t) = case h of
 Instruct l -> if elem h t then h:compactVertical (x+1) m else h : compactVertical (x+1) t
 otherwise -> h:compactVertical (x+1) t
 where m = compactVerticalAux x h t

-- |Função aux  que recebe o indice do primeiro corredor que tem pelo menos um igual na lista e verifica na lista toda os que são iguais e substitui estes por /Repeat/ /indice/
compactVerticalAux :: Int -- ^ recebe uma posição
 -> Instruction -- ^ recebe uma instrução
 ->  Instructions -- ^ recebe as instruçoes onde sabemos que existe uma instrução repetida
 -> Instructions -- ^ Devolve as intruçoes com o respetivo Repeat
compactVerticalAux x _ [] = []
compactVerticalAux x k (h:t)
 |k == h = Repeat x : compactVerticalAux x k t
 |otherwise = h : compactVerticalAux x k t




-- |Função que recebe uma lista de (Int,Piece) e transforma numa Instruction
compactcorridor :: [(Int,Piece)] -> Instruction
compactcorridor l = Instruct l

-- | Função que compacta primeiro os corredores horizontais (muda para instructs e junta) e depois compacta os restantes corredores
compactMazeAux :: Maze -- ^ recebe um labirinto
 -> Instructions -- ^ dá uma lista de instruçoes com os respetivos corredores compactados (
compactMazeAux [] = []
compactMazeAux (h:t) = compactcorridor (juntaseguidos (compactcorridoraux h)) : compactMazeAux t

-- | Função final em que dá o labirinto compactado os corredores todos (Verticais) utilizando o 0 como valor inicial para o acumulador.
compactMaze :: Maze -- ^ recebe um labirinto
 -> Instructions -- ^ dá o labirinto em respetivas Instruções com sinalização nas repetiçoes.
compactMaze l = compactVertical 0 (compactMazeAux l)
