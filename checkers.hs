import Data.List.Split
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]
				
data Pawn = White | Blue | QWhite | QBlue | Empty

initialBoard = [[Empty,Blue,Empty,Blue,Empty,Blue,Empty,Blue],
				[Blue,Empty,Blue,Empty,Blue,Empty,Blue,Empty],
				[Empty,Blue,Empty,Blue,Empty,Blue,Empty,Blue],
				[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
				[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
				[White,Empty,White,Empty,White,Empty,White,Empty],
				[Empty,White,Empty,White,Empty,White,Empty,White],
				[White,Empty,White,Empty,White,Empty,White,Empty]]

instance Show Pawn where
	show White = "w" 
	show Blue = "b"
	show QWhite = "W"
	show QBlue = "B"
	show Empty = "."

data Board = Board [[Pawn]] deriving Show

size :: Int
size = 8

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

setPos (x,y) board pawn = take (y-1) board ++ (setX x (board !! (y-1)) pawn):[] ++ drop y board
        
removePos (x,y) board = setPos (x,y) board Empty
	
