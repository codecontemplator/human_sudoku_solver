-- http://www.sudokudragon.com/sudokustrategy.htm

import Data.List
--(`\\`)
import Data.Char(ord)
import Data.Maybe(fromJust, isJust)

type Position = (Int,Int,Int)
type ValueSet = [Int]
type Cell = (Position,Maybe Int)
type Board = [Cell]
data Group = Row Int | Col Int | Block Int deriving(Show,Eq)
type Strategy = Board -> [Cell]

get_block :: Int -> Int -> Int
get_block c r = quot c 3 + (quot r 3) * 3

cell_position :: Cell -> Position 
cell_position = fst

cell_value :: Cell -> Int
cell_value = fromJust . snd

cell_groups :: Cell -> [Group]
cell_groups ((c,r,b),_) = Col c : Row r : Block b : []

all_groups :: [Group]
all_groups = map Row [0..8] ++ map Col [0..8] ++ map Block [0..8]

cells_in_group :: Group -> Board -> [Cell]
cells_in_group (Row r1) = filter (\((c,r,b),_)->r==r1)
cells_in_group (Col c1) = filter (\((c,r,b),_)->c==c1)
cells_in_group (Block b1) = filter (\((c,r,b),_)->b==b1)

split_group_by_distinct :: Group -> Board -> ([Cell],[Cell])
split_group_by_distinct group board = 
	partition (\(_,v)->isJust v) (cells_in_group group board)

is_distinct :: Cell -> Bool
is_distinct (_,Just _) = True
is_distinct _ = False

distincts_in_group :: Board -> Group -> [Cell]
distincts_in_group board group = filter is_distinct (cells_in_group group board)

positions_in_group :: Group -> [Position]
positions_in_group (Row r1) = [(c,r,b) | let r=r1, c<-[0..8], let b=get_block c r]
positions_in_group (Col c1) = [(c,r,b) | let c=c1, r<-[0..8], let b=get_block c r]
positions_in_group (Block b1) = [(c,r,b) | c<-[0..8], r<-[0..8], let b=get_block c r, b==b1]

all_positions :: [Position]
all_positions = [(c,r,b) | c <- [0..8], r <- [0..8], let b = get_block c r ]

positions_of_non_distincts :: Board -> [Position]
positions_of_non_distincts board = [ p | (p,vs) <- board, not(is_distinct (p,vs))]

onlyChoice :: Strategy
onlyChoice board = 
    [ (unalloc_pos d g, Just(unalloc_val d)) | g <- all_groups, let d = distincts_in_group board g, length d == 8 ]
    where 
        unalloc_pos :: [Cell] -> Group -> Position
        unalloc_pos cells g = head $ positions_in_group g \\ map cell_position cells
        unalloc_val :: [Cell] -> Int
        unalloc_val cells = head $ [1..9] \\ (map cell_value cells)

row_values :: Position -> Board -> ValueSet
row_values (c1,r1,b1) board = map cell_value $ filter (\((c,r,b),_)->c==c1&&r/=r1) $ filter is_distinct board

col_values :: Position -> Board -> ValueSet
col_values (c1,r1,b1) board = map cell_value $ filter (\((c,r,b),_)->r==r1&&c/=c1) $ filter is_distinct board

block_values :: Position -> Board -> ValueSet
block_values (c1,r1,b1) board = map cell_value $ filter (\((c,r,b),_)->not(r==r1&&c==c1)&&b1==b) $ filter is_distinct board

singlePossibility :: Strategy
singlePossibility board = 
	[ (p, Just (head vs)) | p <- positions_of_non_distincts board, let vs = [1..9] \\ (row_values p board ++ col_values p board ++ block_values p board), length vs == 1 ]

onlySquare :: Strategy
onlySquare board = concat $
	[ [(p1,Just(head vs1)), (p2, Just(head vs2))] | 
			   g <- all_groups, 
			   let (d, nd) = split_group_by_distinct g board,
			   length nd == 2,			   
			   nd1 <- nd,
			   let (p1,_) = nd1,
			   let (p2,_) = head $ nd \\ [nd1],
			   let vs1 = [1..9] \\ (row_values p1 board ++ col_values p1 board ++ block_values p1 board),
			   length vs1 == 1,
			   let vs2 = [1..9] \\ (vs1 ++ (map cell_value d))
			   ]

is_allowed :: Board -> Int -> Position -> Bool
is_allowed board value pos = not $ elem value $ row_values pos board ++ col_values pos board ++ block_values pos board 

two_out_of_three :: Strategy
two_out_of_three board = nub $
	[ (vcand, Just v) | 
		gset <- [[Row 0, Row 1, Row 2], [Row 3, Row 4, Row 5], [Row 6, Row 7, Row 8],
		         [Col 0, Col 1, Col 2], [Col 3, Col 4, Col 5], [Col 6, Col 7, Col 8]],
		v <- [1..9],
		g1 <- gset, g2 <- gset, g3 <- gset,
		g1 /= g2, g1 /= g3, g2 /= g3,
		let g1d = distincts_in_group board g1,
		let g2d = distincts_in_group board g2,
		let (g3d, g3nd) = split_group_by_distinct g3 board,
		elem v (map cell_value g1d), elem v (map cell_value g2d), not(elem v (map cell_value g3d)),
		let vcands = filter (is_allowed board v) (map cell_position g3nd),
		length vcands == 1,
		let vcand = head vcands
	]

-----------------------------------------------------------------------------

string2board :: String -> Board
string2board s = map char2cell (zip [0..] s)
	where 
		char2cell (i,ch) = ((c,r,get_block c r), vs)
			where (c, r) = (i `mod` 9, i `quot` 9)
			      vs = if ch == '.' then Nothing else Just (ord ch - ord '0')

sample1 :: Board
sample1 = string2board $ 
	"...7...58" ++
	".56218793" ++
	"......1.." ++
	".......81" ++
	"...376..." ++
	"96......." ++
	"..5......" ++
	"..4.2183." ++
	"87...3..."

sample2 :: Board
sample2 = string2board $
	"825631974" ++
	".67.24..8" ++
	"4....76.2" ++
	".59.48261" ++
	"1.8269745" ++
	".4.175.8." ++
	"3.14....." ++
	"5....34.." ++
	"294..65.."

sample3 :: Board
sample3 = string2board $
	"...769458" ++
	"456218793" ++
	"7894351.." ++
	"347952681" ++
	"518376..." ++
	"962184375" ++
	"..58.7..." ++
	"684521837" ++
	"87.6.35.."

sample4 :: Board
sample4 = string2board $
	"..951..62" ++
	"634...59." ++
	"1256397.4" ++
	"25.84763." ++
	"46..5..17" ++
	".87361.25" ++
	"5.6173248" ++
	".12...976" ++
	"74..961.."

test1 = onlyChoice sample1 == [((0,1,0),Just 4)]
test2 = singlePossibility sample2 == [((0,1,0),Just 9),((2,2,0),Just 3),((0,3,3),Just 7),((3,3,4),Just 3),((1,4,3),Just 3),((0,5,3),Just 6),((6,5,5),Just 3),((5,6,7),Just 2),((6,6,8),Just 8),((2,7,6),Just 6)]
test3 = onlySquare sample3 == [((2,8,6),Just 1),((2,0,0),Just 3)]
test4 = two_out_of_three sample4 == [((8,1,2),Just 1),((6,0,2),Just 3),((5,0,1),Just 4),((2,3,3),Just 1),((2,4,3),Just 3),((6,5,5),Just 4),((6,4,5),Just 8),((3,8,7),Just 2),((1,6,6),Just 9),((1,0,0),Just 7),((5,7,7),Just 5),((3,1,1),Just 7),((3,4,4),Just 9),((7,8,8),Just 5),((8,3,5),Just 9)]
