import Data.List
import Data.Char
import Data.List.Split
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

directions = [(1,1), (-1,1), (-1,-1), (1,-1)]

tupSum (x,y) (x1,y1) = (x+x1, y+y1)

size :: Int
size = 8

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

onBoard (x,y)
	|	(x > 0) && (x <=size) && (y > 0) && (y<=size) = True
	|	otherwise = False
				
data Pawn = White | Blue | QWhite | QBlue | Empty deriving Eq

initialBoard = [[Empty,Blue,Empty,Blue,Empty,Empty,Empty,Empty],
				[Blue,Empty,Blue,Empty,Empty,Empty,Empty,Empty],
				[Empty,QWhite,Empty,Empty,Empty,Empty,Empty,Empty],
				[Empty,Empty,Blue,Empty,Blue,Empty,Blue,Empty],
				[Empty,Blue,Empty,Empty,Empty,Empty,Empty,Empty],
				[QWhite,Empty,QWhite,Empty,Empty,Empty,Blue,Empty],
				[Empty,Empty,Empty,QWhite,Empty,Empty,Empty,Empty],
				[White,Empty,White,Empty,Empty,Empty,White,Empty]]

testingBoard = [[Empty,Blue,Empty,Blue,Empty,Blue,Empty,Blue],
				[Blue,Empty,Blue,Empty,Blue,Empty,Blue,Empty],
				[Empty,Blue,Empty,Blue,Empty,Blue,Empty,Blue],
				[Empty,Empty,White,Empty,Empty,Empty,Empty,Empty],
				[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
				[White,Empty,White,Empty,White,Empty,Empty,Empty],
				[Empty,White,Empty,White,Empty,White,Empty,Blue],
				[White,Empty,White,Empty,White,Empty,Empty,Empty]]

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

reverseDirection d dir
	|	(d == (1,1)) = delete (-1,-1) dir
	|	(d == (-1,-1)) = delete (1,1) dir
	|	(d == (1,-1)) = delete (-1,1) dir
	| 	(d == (-1,1)) = delete (1,-1) dir

setX x row pawn = take (x-1) row ++ (pawn:[]) ++ drop x row

setPos (x,y) pawn board = take (y-1) board ++ (setX x (board !! (y-1)) pawn):[] ++ drop y board
        
removePos (x,y) board = setPos (x,y) Empty board

getPawn (x,y) board = (board !! (y-1)) !! (x-1)

move (x,y) (x1, y1) board = setPos (x1,y1) (getPawn (x,y) board) (setPos(x,y) Empty board) 

parseToArray [] = []
parseToArray (x:xs) = [(fst x)] ++ parseToArray xs  

listBestResults [] _ = []
listBestResults (x:xs) m
	| 	(snd (fst x) == m) = (fst (fst x), snd x) : listBestResults xs m
	| 	otherwise = listBestResults xs m

maxAndListBestResults x = listBestResults x (maximum (map snd (map fst x)))

reduceDuplicates [] = []
reduceDuplicates (x:xs) 
	|	x `elem` xs = reduceDuplicates xs
	| 	otherwise = x : reduceDuplicates xs

checkPossiblePos [] [] _ QBlue _ _ board =  []
checkPossiblePos [] [] _ QWhite _ _ board = []
checkPossiblePos [] [] _ _ last jump board = (zip (zip last [jump]) [board]) ++ []
checkPossiblePos (p:ps) (d:ds) z pawn last jump board 
	|	(pawn == Blue) && (onBoard p) && ((getPawn p board == White) || (getPawn p board == QWhite)) && (onBoard (tupSum p d)) && (getPawn (tupSum p d) board == Empty) = 
		checkPossiblePos ps ds z pawn [] jump board ++ checkPossiblePos (map (tupSum (tupSum p d)) directions) directions 9 pawn [(tupSum p d)] (jump+1) (setPos (tupSum p d) pawn (setPos p Empty board))
	|	(pawn == White) && (onBoard p) && ((getPawn p board == Blue) || (getPawn p board == QBlue)) && (onBoard (tupSum p d)) && (getPawn (tupSum p d) board == Empty) = 
		checkPossiblePos ps ds z pawn [] jump board ++ checkPossiblePos (map (tupSum (tupSum p d)) directions) directions 9 pawn [(tupSum p d)] (jump+1) (setPos (tupSum p d) pawn (setPos p Empty board))
	|	(pawn == QWhite) && (onBoard p) && ((getPawn p board == Blue) || (getPawn p board == QBlue)) && (onBoard (tupSum p d))  && (getPawn (tupSum p d) board == Empty) = 
		(zip (zip [(tupSum p d)] [jump+1]) [(setPos (tupSum p d) pawn (setPos p Empty board))]) ++ checkPossiblePos ps ds 1 pawn [] jump board ++ checkPossiblePos [(tupSum p d)] [d] 1 pawn [p] (jump+1) (setPos p Empty board) ++ checkPossiblePos (map (tupSum (tupSum p d)) (reverseDirection d directions)) (reverseDirection d directions) 0 pawn [(tupSum p d)] (jump+1) (setPos p Empty board)
	|	((pawn == QWhite) || (pawn == QBlue)) && (onBoard p) && (z == 1) && (getPawn p board == Empty) = 
		(zip (zip [p] [jump]) [(setPos p pawn board)])  ++ checkPossiblePos ps ds z pawn last jump board ++ checkPossiblePos [(tupSum p d)] [d] z pawn [p] (jump) board ++ checkPossiblePos (map (tupSum p) (reverseDirection d directions)) (reverseDirection d directions) 0 pawn [] (jump) board	
	|	((snd d) == z) && (onBoard p) && (getPawn p board == Empty) && ((pawn == Blue) || (pawn == White)) =  
		(zip (zip [p] [jump]) [(setPos p pawn board)]) ++ checkPossiblePos ps ds z pawn last jump ((setPos p Empty board))
	| 	otherwise = checkPossiblePos ps ds z pawn last jump board

findPossiblePos (x,y) pawn@Blue board =  checkPossiblePos (map (tupSum (x,y)) directions) directions 1 pawn [] 0 board 

findPossiblePos (x,y) pawn@White board = checkPossiblePos (map (tupSum (x,y)) directions) directions (-1) pawn [] 0 board

findPossiblePos (x,y) pawn@Empty board = []

findPossiblePos (x,y) pawn@QWhite board = checkPossiblePos (map (tupSum (x,y)) directions) directions 1 pawn [] 0 board

findPossiblePos (x,y) pawn@QBlue board = checkPossiblePos (map (tupSum (x,y)) directions) directions (-1) pawn [] 0 board

-- ustawia damki jesli bialy/niebieski doszedl na drugi koniec planszy
changeRow [] _ = []
changeRow (x:xs) d
	|	(x == Blue) && (d==8) = QBlue : changeRow xs d
	|	(x == White) && (d==1) = QWhite : changeRow xs d
	|	otherwise = x : changeRow xs d

setQ [] _ = []
setQ (x:xs) d = changeRow x d : setQ xs (d+1)

-- get material value of figures
countRowValue gamer [] = 0
countRowValue gamer (x:xs) 
	| (gamer == 1) && (x == Blue) = 1 + countRowValue gamer xs
	| (gamer == 1) && (x == QBlue) = 2 + countRowValue gamer xs
	| (gamer == 2) && (x == White) = 1 + countRowValue gamer xs
	| (gamer == 2) && (x == QWhite) = 2 + countRowValue gamer xs
	| otherwise = countRowValue gamer xs

materialValue _ [] = 0
materialValue gamer (x:xs) = countRowValue gamer x + materialValue gamer xs 

possiblePos (x,y) board = reduceDuplicates (maxAndListBestResults (findPossiblePos (x,y) (getPawn (x,y) board) (setPos (x,y) Empty board)))

listPossiblePos (x,y) board = map fst (possiblePos (x,y) board)
findPossibleBoard _ [] = []
findPossibleBoard (x,y) (d:ds) 
	|	((fst d) == (x,y)) = snd d ++ findPossibleBoard (x,y) ds
	| 	otherwise = findPossibleBoard (x,y) ds

--isFinalState board = 

toTup s = (a,b) where
	a = (ord (head s)) - 48
	b = (ord (last s)) - 48

changePlayer v
	| (v == "1") = "2"
	| (v == "2") = "1"

checkMove x player board
	| (x == False) = gameLoop board player
	| otherwise = putStr "Poprawny ruch\n"

gameLoop board player = do 
	putStr (printBoard board)
	putStr ("Ruch gracza: " ++ player ++ "\n")
	putStr ("Wybierz pionek ( np. 2,3)\n")
	ruch <- getLine
	putStr "Przesun na pozycje: "
	ruch2 <- getLine
	putStr ("Twoj ruch to " ++ ruch ++ " na pozycje " ++ ruch2 ++ "\n")
	let x = (toTup ruch2) `elem` (listPossiblePos (toTup ruch) board)
	checkMove x player board 
	--putStr (printBoard (findPossibleBoard (toTup ruch2) (possiblePos (toTup ruch) board)))
	gameLoop (setQ ((findPossibleBoard (toTup ruch2) (possiblePos (toTup ruch) board))) 1) 	(changePlayer player)
	
main = gameLoop testingBoard "1"

-- TODO gracze, ktory ma wykonac ruch(sprawdzenie pionkow)

