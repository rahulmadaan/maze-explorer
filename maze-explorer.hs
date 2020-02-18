data Coord = Coord (Int, Int) deriving (Show, Eq)
type ExitPoint = Coord
type EntryPoint = Coord
data Direction = North | East | South | West deriving (Eq, Enum)

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
turn180 s = left (left s)

move :: Direction -> Coord -> Coord
move South (Coord (x,y)) = Coord ((succ x), y)
move East (Coord ( x ,y)) =  Coord (x, (succ y))
move North (Coord (x,y)) =  Coord ((pred x), y)
move West (Coord (x,y)) = Coord (x, (pred y))

getEntryPoint :: [([Char], (Int, Int))] -> (Direction, Coord)
getEntryPoint c = filterDirection (head (filter (\(x, y) -> 
  x == (show East) || x == (show West) || x == (show North) || x == (show South)) c))

getExitPoint :: [([Char],(Int, Int))] -> Coord
getExitPoint mapping = do
  let exit = head (filter (\(x, y) -> x == (show Exit)) mapping)
  Coord(snd exit)

parseMaze maze = do 
  let grid = (,) <$> [0,1..2] <*> [0,1..6]
  let mazeWithoutNewLine = filter (\x->x /= "\n") (map (\x-> [x]) maze)
  let mapping = zip mazeWithoutNewLine grid
  let path = map (\(x ,y) -> Coord y) (filter (\(x, y) -> x == (show Empty)) mapping)
  let entryPoint = getEntryPoint mapping
  let exitPoint = getExitPoint mapping
  Maze (path ++ [exitPoint]) entryPoint exitPoint

walk :: [Coord] -> Coord -> Direction -> Coord -> Coord
walk path exitPoint currentDirection currentPosition = do
  let nextPosition = move currentDirection currentPosition
  let turnRight = move (right currentDirection) currentPosition
  let turnLeft = move (left currentDirection) currentPosition
  let turnBack = move (turn180 currentDirection) currentPosition

  if currentPosition == exitPoint
    then do
      currentPosition
    else do
      if not (nextPosition `elem` path)
        then do
          if not (turnRight `elem` path)
            then 
              if not (turnLeft `elem` path)
                then walk path exitPoint (turn180 currentDirection) turnBack 
                else
                  walk path exitPoint (left currentDirection) turnLeft    
            else
              walk path exitPoint (right currentDirection) turnRight
        else walk path exitPoint currentDirection nextPosition
            
explore :: Maze -> Coord
explore (Maze { path = p, entryPoint = entry, exitPoint = exit}) = do
  let currentDirection = (fst entry)
  walk p exit currentDirection (snd entry)
