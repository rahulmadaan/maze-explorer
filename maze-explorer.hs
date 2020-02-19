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

filterDirection :: ([Char], (Int, Int)) -> (Direction, Coord)
filterDirection ("^", (x,y)) = (North, (Coord (x,y)))
filterDirection ("<", (x,y)) = (West, (Coord (x,y)))
filterDirection (">", (x,y)) = (East, (Coord (x,y)))
filterDirection ("v", (x,y)) = (South, (Coord (x,y)))

left :: Direction -> Direction
left North = West
left s = pred s

right :: Direction -> Direction
right West = North
right s = succ s

turn180 :: Direction -> Direction
turn180 = left . left

move :: Direction -> Coord -> Coord
move South (Coord (x,y)) = Coord ((succ x), y)
move East (Coord ( x ,y)) =  Coord (x, (succ y))
move North (Coord (x,y)) =  Coord ((pred x), y)
move West (Coord (x,y)) = Coord (x, (pred y))

getEntryPoint :: [([Char], (Int, Int))] -> (Direction, Coord)
getEntryPoint c = filterDirection (head (filter (\(x, y) -> 
  x == (show East) || x == (show West) || x == (show North) || x == (show South)) c))

getExitPoint :: [([Char],(Int, Int))] -> Coord
getExitPoint mapping = 
  let exit = head (filter (\(x, y) -> x == (show Exit)) mapping)
  in Coord(snd exit)

grid = [(x,y) | x <- [0..2], y <- [0..6]]
mazeWithoutNewLine x = (filter (/= "\n")) (map (\n -> [n]) x)

parseMaze maze = do 
  let mapping = zip (mazeWithoutNewLine maze) grid
  let path = map (Coord . snd) (filter (\(x, y) -> x == (show Empty)) mapping)
  let entryPoint = getEntryPoint mapping
  let exitPoint = getExitPoint mapping
  Maze (path ++ [exitPoint]) entryPoint exitPoint

getDirection :: Direction -> Int -> Direction
getDirection currentDirection stepCount = do
  if ((mod stepCount 6) == 0) 
    then (turn180 currentDirection)
    else currentDirection

walk :: [Coord] -> Coord -> Direction -> Coord -> Int -> Int -> Message
walk _ _ _ _ 3 _ = Impossible
walk path exitPoint currentDirection currentPosition infiniteCount stepCount = do
  let nextPosition = move currentDirection currentPosition
  let turnRight = move (right currentDirection) currentPosition
  let turnLeft = move (left currentDirection) currentPosition
  let turnBack = move (turn180 currentDirection) currentPosition
  let direction = getDirection currentDirection stepCount
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
                then walk path exitPoint (turn180 direction) turnBack (succ infiniteCount) (succ stepCount)
                else
                  walk path exitPoint (left direction) turnLeft infiniteCount (succ stepCount)
            else
              walk path exitPoint (right direction) turnRight infiniteCount (succ stepCount)
        else walk path exitPoint direction nextPosition infiniteCount (succ stepCount)
                
explore :: Maze -> Message
explore (Maze { path = p, entryPoint = entry, exitPoint = exit}) = do
  let currentDirection = (fst entry)
  walk p exit currentDirection (snd entry) 0 0
