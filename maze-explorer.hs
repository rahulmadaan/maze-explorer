data Coord = Coord (Int, Int) deriving (Show, Eq)
type ExitPoint = Coord
type EntryPoint = Coord
data Direction = North | East | South | West deriving (Eq, Enum)
data Message = Possible | Impossible deriving (Eq, Enum)

instance Show Message where 
  show Possible = "possible/true/yay"
  show Impossible = "impossible/not true/darn it"

data Maze = Maze { path :: [Coord],
                   entryPoint :: (Direction, Coord),
                   exitPoint :: (Coord)
                  } deriving (Show)

instance Show Direction where 
  show North = "^"
  show East = ">"
  show South = "v"
  show West = "<"

data Symbol = Exit | Wall | Empty deriving (Eq, Enum)

instance Show Symbol where 
  show Exit = "E"
  show Wall = "#"
  show Empty = " "

getDirection :: String -> Direction
getDirection "^" = North
getDirection "<" = West
getDirection ">" = East
getDirection "v" = South

moveLeft :: Direction -> Direction
moveLeft North = West
moveLeft s = pred s

moveRight :: Direction -> Direction
moveRight West = North
moveRight s = succ s

turn180 :: Direction -> Direction
turn180 = moveLeft . moveLeft

parseCoord :: (Int, Int) -> Coord
parseCoord (x,y) = Coord (x, y)

getEntryPoint :: [(String, (Int, Int))] -> (Direction, Coord)
getEntryPoint maze = 
  let entryPoint = head (filter ( isDirection . fst)  maze)
  in convertToDomain (entryPoint)

move :: Coord -> Direction -> Coord
move (Coord (x,y)) South = Coord ((succ x), y)
move (Coord (x,y)) East =  Coord (x, (succ y))
move (Coord (x,y)) North =  Coord ((pred x), y)
move (Coord (x,y)) West = Coord (x, (pred y))

isDirection :: String -> Bool
isDirection symbol = (any ((symbol==) . show) [North, South, East, West])

getExitPoint :: [(String,(Int, Int))] -> Coord
getExitPoint mapping = 
  let exitPoint = (snd (head (filter (\(x, y) -> x == (show Exit)) mapping)))
  in Coord exitPoint

grid = [(x,y) | x <- [0..2], y <- [0..8]]
mazeWithoutNewLine x = (filter (/= "\n")) (map (\n -> [n]) x)

mapping :: String -> [(String,(Int, Int))]
mapping maze = zip (mazeWithoutNewLine maze) grid

isExit :: String -> Bool
isExit symbol = symbol == (show Exit)

isEmpty :: String -> Bool
isEmpty symbol = symbol == (show Empty)

isPath :: String -> Bool
isPath x = (isDirection x) ||  (isExit x) || (isEmpty x)

convertToDomain :: (String, (Int, Int)) -> (Direction, Coord)
convertToDomain (d, (x,y)) = ((getDirection d), parseCoord(x,y))

parseMaze maze = do
  let path = map (Coord . snd) (filter (isEmpty . fst) (mapping maze))
  let entryPoint = getEntryPoint (mapping maze)
  let exitPoint = getExitPoint (mapping maze)
  Maze ([(snd entryPoint)] ++ path ++ [exitPoint]) entryPoint exitPoint

turn :: Direction -> [Direction]
turn currentDirection = (sequence [moveRight,moveLeft,turn180,id] currentDirection)

data Action = Forward | GoRight | GoLeft | AboutTurn deriving (Show,Eq)

actionToTake True _ _ _ = Forward
actionToTake _ True _ _ = GoRight
actionToTake _ _ True _ = GoLeft
actionToTake _ _ _ True = AboutTurn


walk :: [Coord] -> Coord -> Direction -> Coord -> Int -> Message
walk _ _ _ _ 3 = Impossible
walk path exitPoint currentDirection currentPosition infiniteCount = 
  if currentPosition == exitPoint 
    then 
      Possible
      else
        case action of Forward   -> walkToNext currentDirection forward infiniteCount
                       GoRight   -> walkToNext (moveRight currentDirection) right infiniteCount
                       GoLeft    -> walkToNext (moveLeft currentDirection) left infiniteCount
                       AboutTurn -> walkToNext (turn180 currentDirection) backward (succ infiniteCount)
        where inPath = (`elem` path)
              nextPosition = map (move currentPosition) (turn currentDirection)
              [canMoveRight, canMoveLeft, canMoveBackward, canMoveForward] = map inPath nextPosition
              [right, left, backward, forward] = nextPosition
              action = actionToTake canMoveForward canMoveRight canMoveLeft canMoveBackward
              walkToNext = walk path exitPoint
                        
explore :: Maze -> Message
explore (Maze { path = p, entryPoint = entry, exitPoint = exit}) = do
  let currentDirection = (fst entry)
  walk p exit currentDirection (snd entry) 0
