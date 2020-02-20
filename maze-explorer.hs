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

left :: Direction -> Direction
left North = West
left s = pred s

right :: Direction -> Direction
right West = North
right s = succ s

turn180 :: Direction -> Direction
turn180 = left . left

parseCoord :: (Int, Int) -> Coord
parseCoord (x,y) = Coord (x, y)

getEntryPoint :: [(String, (Int, Int))] -> (Direction, Coord)
getEntryPoint maze = 
  let entryPoint = head (filter ( isDirection . fst)  maze)
  in convertToDomain (entryPoint)

move :: Direction -> Coord -> Coord
move South (Coord (x,y)) = Coord ((succ x), y)
move East (Coord ( x ,y)) =  Coord (x, (succ y))
move North (Coord (x,y)) =  Coord ((pred x), y)
move West (Coord (x,y)) = Coord (x, (pred y))

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

walk :: [Coord] -> Coord -> Direction -> Coord -> Int -> Message
walk _ _ _ _ 3 = Impossible
walk path exitPoint currentDirection currentPosition infiniteCount = do
  let nextPosition = move currentDirection currentPosition
  let turnRight = move (right currentDirection) currentPosition
  let turnLeft = move (left currentDirection) currentPosition
  let turnBack = move (turn180 currentDirection) currentPosition

  let inPath = (`elem` path)
  if currentPosition == exitPoint
    then
      Possible
    else 
      if not (inPath nextPosition) 
        then
          if not (inPath turnRight) 
            then
              if not (inPath turnLeft)
                then
                  walk path exitPoint (turn180 currentDirection) turnBack (succ infiniteCount)
                else
                  walk path exitPoint (left currentDirection) turnLeft infiniteCount
            else 
              walk path exitPoint (right currentDirection) turnRight infiniteCount
        else
          walk path exitPoint currentDirection nextPosition infiniteCount
                
explore :: Maze -> Message
explore (Maze { path = p, entryPoint = entry, exitPoint = exit}) = do
  let currentDirection = (fst entry)
  walk p exit currentDirection (snd entry) 0
