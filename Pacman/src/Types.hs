{-|
Module : Types
Description : apenas contem os tipos
Copyright : João Martins <a91669@alunos.uminho.pt>
            José Neiva <a92945@alunos.uminho.pt>

-}
module Types where

-- | Tarefa1
-- | labirinto
type Maze = [Corridor]
-- | corredor
type Corridor = [Piece]
-- | pecas
data Piece = Food FoodType | Wall | Empty deriving Eq
-- | tipos de comida
data FoodType = Big | Little deriving Eq

-- | instance para show
instance Show Piece where
     show (Food Big) = "o"
     show (Food Little) = "."
     show (Wall) = "#"
     show (Empty) = " "


--Tarefa2
-- | jogada
data Play = Move Int Orientation deriving Show
-- | orientacoes
data Orientation = L | R | U | D | Null deriving (Show,Eq)
-- | coordenadas
type Coords=(Int,Int)


-- | estado
data State = State
      {
          maze :: Maze
      ,   playersState :: [Player]
      ,   level :: Int
      } deriving Show

-- | jogador
data Player  =  Pacman PacState | Ghost GhoState deriving Show

-- | estado do pacman
data PacState = PacState
         {
             pacState :: PlayerState
         ,   timeMega :: Double
         ,   openClosed :: Mouth
         ,   pacmanMode :: PacMode

         } deriving Show

-- | estado fantasma
data GhoState = GhoState
         {
             ghostState :: PlayerState
         ,   ghostMode :: GhostMode
         } deriving Show

-- | estado do jogador
type PlayerState = (Int, Coords, Double , Orientation, Int, Int) --O identificador único;As coordenadas cartesianas (x,y) do jogador no Labirinto;A velocidade;A orientação;A pontuação;O número de vidas restantes.

-- | boca
data Mouth = Open | Closed deriving Show
-- | modo do pacman
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
-- | modo fantasma
data GhostMode = Dead  | Alive deriving (Eq,Show)


--Tarefa 3

-- | instrucoes
type Instructions = [Instruction]
-- | instrucao
data Instruction = Instruct [(Int, Piece)] | Repeat Int deriving (Show,Eq)


