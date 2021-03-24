{-|
Module : Tarefa 1
Description : Módulo Haskell que faz um mapa
Copyright : João Martins <a91669@alunos.uminho.pt>
            José Neiva  >a92945@alunos.uminho.pt>
O objetivo deste módulo e formar um mapa com um comprimento (x) uma altura (y) e uma semente (s).
Um mapa é um conjunto de y corredores com cada um com tamanho x a semente serve de um numero arbritario para o gerador de pseudo random
este modulo forma uma lista de inteiros pseudo random, transforma cada inteiro com a sua respectiva peça e vai formando um corredor.
Um mapa do jogo pacman tem sempre um tunel e uma casa dos fantasma no meio do mapa
que as suas entradas e dimensoes respectivamente, dependem de x e y.
-}
module Tarefa1 where

import System.Random
import Data.List
import Types


-- |Função que cria uma lista de n elementos de numeros random
--
generateRandoms :: Int
      -> Int
      -> [Int]
generateRandoms n seed = let gen = mkStdGen seed
                        in take n $ randomRs (0,99) gen



-- |Função que converte um inteiro para a sua respectiva peca
--
convertePeca :: Int -- ^  recebe um inteiro
          -> Piece -- ^ dá a peça respectiva ao inteiro
convertePeca a
 |a==3 = Food Big
 |a>=70 && a<=99 = Wall
 |otherwise = Food Little



-- | Função que converte uma lista de inteiros para um corredor utilizando a função anterior definida /convertePeca/
converteCorredor :: [Int] -- ^ recebe uma lista de inteiros
     -> Int -- ^ recebe o tamanho da lista (x)
     -> Corridor -- ^ dá uma lista de peças de tamanho x
converteCorredor _ 0 = []
converteCorredor (h:t) x = convertePeca h:converteCorredor t (x-1)



--funcoes auxiliares que dão print para teste
--

-- |Função que transforma um corredor em string
printCorridor :: Corridor -- ^ recebe um corredor com x peças
       -> String -- ^ dá uma string com x caracteres
printCorridor [] = ""
printCorridor (h:t) = show h ++ printCorridor t



-- | Função que torna um Maze em string
--
mazetostring :: Maze -- ^ recebe uma lista de corridores
         -> String -- ^ dá uma string do labirinto
mazetostring [] = ""
mazetostring (h:t) = printCorridor h ++ "\n" ++ mazetostring t

-- |função auxiliar que forma um maze com espacos
printmaze :: Maze -> IO()
printmaze s = putStr(mazetostring s)


-- | Função que forma as paredes, estas que sao corredores so com pecas /Wall/
--
paredes :: Int -- ^ recebe um inteiro x
          -> Corridor -- ^  o resultado e um corredor com x peças (Wall)
paredes 0 = []
paredes x = Wall:paredes(x-1)




-- | Função que cria um corredor generico com uma lista de inteiros que forma todos os corredores do mapa com excecao das paredes e do tunel
corridorValido :: [Int] -- ^ recebe uma lista de int
          -> Int -- ^ recebe um int x
          -> Corridor -- ^ o output vai ser um corridor de tamanho x com wall no inicio e fim
corridorValido l x = [Wall] ++ (converteCorredor l (x-2)) ++ [Wall]



-- | Função que junta varios corredores validos
--
juntaC :: [Int] -- ^ recebe uma lista de int
    -> Int -- ^ recebe o tamanho dos corridores (x)
    -> Int -- ^ recebe o numero de corridores (y)
    -> Maze -- ^ gera uma labirinto com  y corridores validos de tamanho x
juntaC _ _ 0 = []
juntaC l x y  = (corridorValido l x):(juntaC (drop x l) x (y-1))



--Funçoẽs de criação tunel


-- | Função que forma o primeiro corredor do tunel dando drop nos numeros da lista que ja foram anteriormente utilizadas
--
corredor1tunel :: [Int] -- ^ recebe uma lista de inteiros
       -> Int  -- ^ recebe o tamanho do corredor x
       -> Corridor -- ^ dá o primeiro corredor do tunel de tamanho x
corredor1tunel l x
 | mod x 2 == 0 = [Wall] ++ (converteCorredor l ((div x 2 )-6)) ++ [Empty] ++ (replicate 3 Wall) ++  (replicate 2 Empty) ++   (replicate 3 Wall) ++ [Empty] ++ (converteCorredor (drop 6 l) ((div x 2 )-6)) ++ [Wall]
 |otherwise = [Wall] ++ (converteCorredor l ((div x 2 )-6)) ++ [Empty] ++ (replicate 3 Wall) ++  (replicate 3 Empty) ++ (replicate 3 Wall) ++ [Empty] ++ (converteCorredor (drop ((div x 2 )-6) l) ((div x 2 )-6)) ++ [Wall]



-- | Função que forma o segundo corredor do tunel dando drop nos numeros da lista que ja foram anteriormente utilizadas
--
corredor2tunel :: [Int] -- ^ recebe uma lista de inteiros
            -> Int -- ^ recebe o tamando do corridor x
            -> Corridor -- ^ dá o segundo corridor do tunel de tamanho x
corredor2tunel l x
 | mod x 2 == 0 = [Empty] ++ (converteCorredor (drop x2 l) ((div x 2 )-6)) ++ [Empty] ++ [Wall] ++ (replicate 6 Empty) ++ [Wall] ++ [Empty] ++ (converteCorredor (drop x3 l) ((div x 2 )-6)) ++ [Empty]
 |otherwise  = [Empty] ++ (converteCorredor (drop x2 l) ((div x 2 )-6)) ++ [Empty] ++ [Wall] ++ (replicate 7 Empty) ++ [Wall] ++ [Empty] ++ (converteCorredor (drop x3 l) ((div x 2 )-6)) ++ [Empty]
  where x2 = 2*((div x 2 )-6)
        x3 = 3*((div x 2 )-6)



-- | Função que forma o segundo corredor do tunel no caso de o tunel ter só uma entrada dando drop nos numeros da lista que ja foram anteriormente utilizadas
--
corredor3tunel :: [Int] -- ^ recebe uma lista de inteiros
             -> Int -- ^ recebe o tamanho do corridor x
             -> Corridor -- ^ dá o terceiro corridor de tamanho x
corredor3tunel l x
  |mod x 2 == 0 = [Wall] ++ (converteCorredor (drop x2 l) ((div x 2 )-6))  ++ [Empty] ++ (replicate 8 Wall) ++ [Empty] ++ (converteCorredor (drop x3 l) ((div x 2 )-6)) ++ [Wall]
  |otherwise = [Wall] ++ (converteCorredor (drop x2 l) ((div x 2 )-6))  ++ [Empty] ++ (replicate 9 Wall) ++ [Empty] ++ (converteCorredor (drop x3 l) ((div x 2 )-6)) ++ [Wall]
  where x2 = 4*((div x 2 )-6)
        x3 = 5*((div x 2 )-6)



-- | Função que forma o segundo corredor do tunel no caso de o tunel ter duas entradas dando drop nos numeros da lista que ja foram anteriormente utilizadas
--
corredor3tunelC :: [Int] -- ^ recebe uma lista de inteiros
             -> Int -- ^ recebe o tamanho do corridor x
             -> Corridor -- ^ dá o segundo  corridor do tunel de tamanho x
corredor3tunelC l x
  |mod x 2 == 0 = [Empty] ++ (converteCorredor (drop x2 l) ((div x 2 )-6))  ++ [Empty] ++ (replicate 8 Wall) ++ [Empty] ++ (converteCorredor (drop x3 l) ((div x 2 )-6)) ++ [Empty]
  |otherwise = [Empty] ++ (converteCorredor (drop x2 l) ((div x 2 )-6))  ++ [Empty] ++ (replicate 9 Wall) ++ [Empty] ++ (converteCorredor (drop x3 l) ((div x 2 )-6)) ++ [Empty]
  where x2 = 4*((div x 2 )-6)
        x3 = 5*((div x 2 )-6)



-- | Função que forma o corredor anterior ao inicio do tunel dando drop nos numeros da lista que ja foram anteriormente utilizadas
--
corredorpvazias :: [Int] -- ^ recebe uma lista de inteiros
               -> Int -- ^ recebe o tamanho do corridor x
               -> Corridor -- ^ dá um corridor de tamanho x
corredorpvazias l x
 |mod x 2 == 0 = [Wall] ++ (converteCorredor l ((div x 2 )-6)) ++ (replicate 10 Empty) ++  (converteCorredor (drop ((div x 2 )-6) l) ((div x 2 )-6)) ++ [Wall]
 |otherwise = [Wall] ++ (converteCorredor l ((div x 2 )-6)) ++ (replicate 11 Empty) ++ (converteCorredor (drop ((div x 2 )-6) l) ((div x 2 )-6)) ++ [Wall]



-- |Função que forma o tunel completo no caso de ter apenas uma entrada
--
juntatunel :: [Int] -- ^ recebe uma lista de inteiros
          -> Int -- ^ recebe o numero de corridores x
          -> Maze -- ^ da x corridores compactados
juntatunel l x = [corredorpvazias l x] ++ [corredor1tunel l x] ++ [corredor2tunel l x] ++ [corredor3tunel l x] ++ [corredorpvazias l2 x]
  where l2 = (drop (6*((div x 2 )-6)) l)


-- |Função que forma o tunel completo no caso de ter duas entradas
--
juntatunelC :: [Int] -- ^ recebe uma lista de inteiros
          -> Int -- ^ recebe o numero de corridores x
          -> Maze -- ^ da x corridores compactados
juntatunelC l x = [corredorpvazias l x] ++ [corredor1tunel l x] ++ [corredor2tunel l x] ++ [corredor3tunelC l x] ++ [corredorpvazias l2 x]
  where l2 = (drop (6*((div x 2 )-6)) l)


-- | Função que respetivamente analiza se as variaveis são pares ou impares e manda para as funções corretas
--
generateMazeAux :: [Int] -- ^ recebe uma lista de inteiros
              -> Int -- ^ recebe um inteiro para analisar se e par ou nao
              -> Int -- ^ recebe um inteiro para analisar se e par ou nao
              -> Maze -- ^ gera um labirinto com as respetivas funçoes
generateMazeAux l x y
 |(mod x 2 == 0) && (mod y 2 == 0) = [paredes x] ++ (juntaC l x ((div y 2)-4)) ++ (juntatunelC (drop ((x-2)*((div y 2)-4)) l) x) ++ (juntaC (drop x3 l) x ((div y 2)-3)) ++ [paredes x]
 |(mod x 2 == 1) && (mod y 2 == 0) = [paredes x] ++ (juntaC l x ((div y 2)-4)) ++ (juntatunelC (drop ((x-2)*((div y 2)-4)) l) x) ++ (juntaC (drop x3 l) x ((div y 2)-3)) ++ [paredes x]
 |(mod x 2 == 0) && (mod y 2 == 1) = [paredes x] ++ (juntaC l x ((div y 2)-3)) ++ (juntatunel (drop ((x-2)*((div y 2)-3)) l) x) ++ (juntaC (drop x3 l) x ((div y 2)-3)) ++ [paredes x]
 |(mod x 2 == 1) && (mod y 2 == 1) = [paredes x] ++ (juntaC l x ((div y 2)-3)) ++ (juntatunel (drop ((x-2)*((div y 2)-3)) l) x) ++ (juntaC (drop x3 l) x ((div y 2)-3)) ++ [paredes x]
    where x3=((x-2)*((div y 2)-4))+(4*(div x 2 )-6)



-- |Função que gera a lista de numeros pseudo random e envia para a generateMazeAux com o comprimento e altura do labirinto para ser devidamente analizada
--
generateMaze :: Int -- ^ recebe uma lista de inteiros
              -> Int -- ^ recebe o comprimento x
              -> Int -- ^ recebe o numero de corridores y
              -> Maze -- ^ gera o labirinto final com as dimensoes x y
generateMaze x y seed = generateMazeAux (generateRandoms ((x*y)-((x-2)-2*y-11*4)) seed) x y
