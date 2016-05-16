import Data.List.Split
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]

directions = [(1,1), (-1,1), (-1,-1), (1,-1)]

tupSum (x,y) (x1,y1) = (x+x1, y+y1)

size :: Int
size = 8

onBoard (x,y)
	|	(x > 0) && (x <=size) && (y > 0) && (y<=size) = True
	|	otherwise = False
				
data Pawn = White | Blue | QWhite | QBlue | Empty deriving Eq

initialBoard = [[Empty,Blue,Empty,Blue,Empty,Blue,Empty,Blue],
				[Blue,Empty,Blue,Empty,Blue,Empty,Blue,Empty],
				[Empty,White,Empty,White,Empty,Blue,Empty,Blue],
				[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
				[Empty,Empty,Empty,White,Empty,White,Empty,Empty],
				[White,Empty,Empty,Empty,White,Empty,Empty,Empty],
				[Empty,White,Empty,White,Empty,Empty,Empty,White],
				[White,Empty,White,Empty,Empty,Empty,White,Empty]]

testingBoard = [[Empty,Blue,Empty,Blue,Empty,Blue,Empty,Blue],
				[Blue,Empty,Blue,Empty,Blue,Empty,Blue,Empty],
				[Empty,Blue,Empty,Blue,Empty,Blue,Empty,Blue],
				[Empty,Empty,White,Empty,Empty,Empty,Empty,Empty],
				[Empty,Empty,Empty,Empty,Empty,White,Empty,Empty],
				[White,Empty,Empty,Empty,Blue,Empty,White,Empty],
				[Empty,White,Empty,White,Empty,White,Empty,White],
				[White,Empty,White,Empty,White,Empty,White,Empty]]

instance Show Pawn where
	show White = "w" 
	show Blue = "b"
	show QWhite = "W"
	show QBlue = "B"
	show Empty = "."

data Board = Board [[Pawn]] deriving Show

printRow [] = ""
printRow (x:xs) = show x ++ "" ++ printRow xs

printBoard [] = ""
printBoard (x:xs) = printRow x ++ "\n" ++ printBoard xs

parseStr "" = []
parseStr (x:xs) = x : parseStr xs

initRow [] = []
initRow (x:xs) 
	|	(x == '.') = Empty : initRow xs
	|	(x == 'w') = White : initRow xs
	|	(x == 'b') = Blue : initRow xs
	| 	(x == 'B') = QBlue : initRow xs
	| 	(x == 'W') = QWhite : initRow xs
	| 	otherwise = []

parseToBoard [] = []
parseToBoard (x:xs) = initRow x : parseToBoard xs
initBoard x = parseToBoard(splitOn "\n" x) 
--instance (Read a) => Read (Board a) where
  --  readsPrec _ value = readsBoard value
setX x row pawn = take (x-1) row ++ (pawn:[]) ++ drop x row

setPos (x,y) pawn board = take (y-1) board ++ (setX x (board !! (y-1)) pawn):[] ++ drop y board
        
removePos (x,y) board = setPos (x,y) Empty board

getPawn (x,y) board = (board !! (y-1)) !! (x-1)

move (x,y) (x1, y1) board = setPos (x1,y1) (getPawn (x,y) board) (setPos(x,y) Empty board) 

checkPossiblePos [] [] _ _ last _ = last ++ []
checkPossiblePos (p:ps) (d:ds) z pawn last board 
	|	(pawn == Blue) && (onBoard p) && ((getPawn p board == White) || (getPawn p board == QWhite)) && (onBoard (tupSum p d)) && (getPawn (tupSum p d) board == Empty) = checkPossiblePos ps ds z pawn [] board ++ checkPossiblePos (map (tupSum (tupSum p d)) directions) directions 9 pawn [(tupSum p d)] (setPos p Empty board)
	|	(pawn == White) && (onBoard p) && ((getPawn p board == Blue) || (getPawn p board == QBlue)) && (onBoard (tupSum p d)) && (getPawn (tupSum p d) board == Empty) = checkPossiblePos ps ds z pawn [] board ++ checkPossiblePos (map (tupSum (tupSum p d)) directions) directions 9 pawn [(tupSum p d)] board
	|	((snd d) == z) && (onBoard p) && (getPawn p board == Empty) =  p :checkPossiblePos ps ds z pawn last (setPos p Empty board)
	| 	otherwise = checkPossiblePos ps ds z pawn last board

findPossiblePos (x,y) pawn@Blue board =  checkPossiblePos (map (tupSum (x,y)) directions) directions 1 pawn [] board 

findPossiblePos (x,y) pawn@White board =  checkPossiblePos (map (tupSum (x,y)) directions) directions (-1) pawn [] board 

possiblePos (x,y) board = findPossiblePos (x,y) (getPawn (x,y) board) board

--	|	(onBoard p) && ((getPawn p board == White) || (getPawn p board == QWhite)) && (onBoard (tupSum p d)) && (getPawn (tupSum p d) board == Empty) = (tupSum p d) : checkPossiblePos ps ds z pawn board
--	|	(onBoard p) && ((getPawn p board == Blue) || (getPawn p board == QBlue)) && (onBoard (tupSum p d)) && (getPawn (tupSum p d) board == Empty) = (tupSum p d) : checkPossiblePos ps ds z pawn board
	
