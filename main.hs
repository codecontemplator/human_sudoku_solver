-- http://www.sudokudragon.com/sudokustrategy.htm
-- http://www.sudoku-solutions.com/

-----------------------------------------------------------------------------
-- imports
-----------------------------------------------------------------------------

import Data.List
--(`\\`, unlines)
import Data.Char(digitToInt, intToDigit)
import Data.Maybe(fromJust, isJust, isNothing)
import Debug.Trace(trace)
import Data.List.Split(chunksOf)
import Data.Map(fromListWith, toList)

-----------------------------------------------------------------------------
-- types
-----------------------------------------------------------------------------

type Position = (Int,Int,Int)
type ValueSet = [Int]
type Cell = (Position,ValueSet)
type Board = [Cell]
data Group = Row Int | Col Int | Block Int deriving(Show,Eq)
type Strategy = Board -> [Cell]

-----------------------------------------------------------------------------
-- sudoku fundamentals
-----------------------------------------------------------------------------

get_block :: Int -> Int -> Int
get_block c r = quot c 3 + (quot r 3) * 3

cell_position :: Cell -> Position 
cell_position = fst

cell_value :: Cell -> Int
cell_value (_, [v]) = v
cell_value _ = error "cell_value called for non-distinct cell"

is_distinct :: Cell -> Bool
is_distinct (_,[_]) = True
is_distinct _ = False

is_shared :: Position -> Position -> Bool
is_shared (c1,r1,b1) (c,r,b) = c==c1&&r/=r1 || r==r1&&c/=c1 || not(r==r1&&c==c1)&&b1==b

is_allowed :: Board -> Int -> Position -> Bool
is_allowed board value pos = not $ elem value $ [ v | c@(p,[v]) <- board, is_distinct c, is_shared pos p ]

is_complete :: Board -> Bool
is_complete board = all is_distinct board

is_group_member :: Group -> Position -> Bool
is_group_member (Row r1) (r,_,_) = r1 == r
is_group_member (Col c1) (_,c,_) = c1 == c
is_group_member (Block b1) (_,_,b) = b1 == b

--group_members :: Group -> [Cell] -> [Cell]
--group_members g = filter (\(p,_) -> is_group_member g p)

all_groups :: [Group]
all_groups = map Row [0..8] ++ map Col [0..8] ++ map Block [0..8]

-------------------------------------------------------------------------------------------------
-- formatting / io
-------------------------------------------------------------------------------------------------

board2string :: Board -> Bool -> String
board2string board format =
	let 
		cell_positions = concatMap (\r -> map (\c -> (c, r, get_block c r)) [0..8]) [0..8]
		cell_values = chunksOf 9 $ map (\p -> fromJust (lookup p board)) cell_positions		
		v2ch :: ValueSet -> Char
		v2ch v = case v of
					[v'] -> intToDigit v'
					_ -> '.'					
		formatter = if format then unlines else concat
	in
		formatter $ map (\r -> map v2ch r) cell_values

displayBoard :: Board -> IO()
displayBoard board = putStrLn $ board2string board True

string2board :: String -> Board
string2board s = map char2cell (zip [0..] s)
	where 
		char2cell (i,ch) = ((c,r,get_block c r), vs)
			where (c, r) = (i `mod` 9, i `quot` 9)
			      vs = if ch == '.' then [1..9] else [ digitToInt ch ] -- (ord ch - ord '0')

-------------------------------------------------------------------------------------------------
-- strategies
-------------------------------------------------------------------------------------------------

--
-- Only choice
--
-- There may be only one possible choice for a particular Sudoku square. In the simplest case 
-- you have a group (row, column or region) that has eight squares allocated leaving only one 
-- remaining choice available; so the remaining number must go in that empty square.
--
-- ref: http://www.sudokudragon.com/sudokustrategy.htm
-- 
only_choice :: Strategy
only_choice board = 
    [ (p, v) | 
    	g <- all_groups, 
    	let gm = [ c | c@(p,_) <- board, is_group_member g p],
    	let (d,nd) = partition is_distinct gm, 
    	length nd == 1,
    	let [(p,_)] = nd,
    	let v = [1..9] \\ (map cell_value d)
    ]

--
-- Naked single / single possibility
--
-- The "naked single" solving technique also known as "singleton" or "lone number" 
-- is one of the simplest Sudoku solving techniques. Using this technique the candidate 
-- values of an empty cell are determined by examining the values of filled cells in the 
-- row, column and box to which the cell belongs. If the empty cell has just one single 
-- candidate value then this must be the value of the cell.
--
-- ref: http://www.sudoku-solutions.com/solvingNakedSubsets.php#nakedSingle
--
naked_single :: Strategy
naked_single board = 
	[ (p, vs) | 
		let (d,nd) = partition is_distinct board,
		(p,_) <- nd,
		let vs = [1..9] \\ [ v1 | (p1,[v1]) <- d, is_shared p p1],
		length vs == 1 
	]


--
-- Only square (I find no point in using this strategy - use naked single instead)
--
-- Often you will find within a group of Sudoku squares that there is only one place that 
-- can take a particular number. For example if a group has seven squares allocated with only 
-- two numbers left to allocate it is often the case that an intersecting (or shared) group forces 
-- a number to go in one of the squares and not the other one. You are left with an 'only square' 
-- within a group for a number to go in. This is different to the 'single possibility' rule where we 
-- looked at squares on their own rather than as a group.
--
-- ref: http://www.sudokudragon.com/sudokustrategy.htm
--
only_square :: Strategy
only_square board = concat $
	[ [(p1,vs1), (p2, vs2)] | 
			   g <- all_groups, 
		   	   let gm = [ c | c@(p,_) <- board, is_group_member g p ],
			   let (d, nd) = partition is_distinct gm,
			   length nd == 2,			   
			   (p1,_) <- nd, (p2,_) <- nd, p1 /= p2,	
			   let vs1 = [1..9] \\ [ v | c@(p,[v]) <- board, is_distinct c, is_shared p1 p],
			   length vs1 == 1,
			   let vs2 = [1..9] \\ (vs1 ++ (map cell_value d))
	]

-- 
-- Two out of three
--
-- The two lines out of three lines strategy is one of the most useful Sudoku strategies. 
-- It finds most of the simplest to solve squares and can be used in a systematic manner to 
-- clear up the first and last few squares in the puzzle.
-- Take three lines (rows or columns) in a region. Look for the occurrences of a particular 
-- symbol, lets say '5' in these lines. If you find three occurrences then the symbol is 'solved' 
-- in those lines so move on to the next set of three. However if you find two then that automatically 
-- narrows down where the remaining '5' can occur, it can not occur in the two lines you have found 
-- containing the '5' or the regions of three squares in which the '5' occurs. You have narrowed 
-- the search so '5' must be in one of three squares. It's often the case that one or two of these 
-- squares are already filled so you can work out simply in which square the '5' must go. If you 
-- do this for each group of three rows then all groups of three columns you soon scan the whole 
-- grid for one symbol and then simply repeat for each symbol.
--
-- ref: http://www.sudokudragon.com/forum/twothreestrategy.htm
--
two_out_of_three :: Strategy
two_out_of_three board = nub $
	[ (vp, [v]) | 
		gset <- [[Row 0, Row 1, Row 2], [Row 3, Row 4, Row 5], [Row 6, Row 7, Row 8],
		         [Col 0, Col 1, Col 2], [Col 3, Col 4, Col 5], [Col 6, Col 7, Col 8]],
		v <- [1..9],
		g1 <- gset, g2 <- gset, g3 <- gset,
		g1 /= g2, g1 /= g3, g2 /= g3,
		let g1d = [ c | c@(p,_) <- board, is_distinct c, is_group_member g1 p ],
		let g2d = [ c | c@(p,_) <- board, is_distinct c, is_group_member g2 p ],
		let (g3d, g3nd) = partition is_distinct [ c | c@(p,_) <- board, is_group_member g3 p ],
		elem v (map cell_value g1d), elem v (map cell_value g2d), not(elem v (map cell_value g3d)),
		let valid_positions_for_v = [ p | (p,_) <- g3nd, is_allowed board v p ],
		length valid_positions_for_v == 1,
		let vp = head valid_positions_for_v
	]

-----------------------------------------------------------------------------
-- solver
-----------------------------------------------------------------------------

propagate_constraints :: Board -> Cell -> Board
propagate_constraints board c@(p,v) = 
	let
		(shared, non_shared) = partition (is_shared p . cell_position) board
		shared' = map (\(p1,vs) -> (p1, vs \\ v)) shared
	in
		shared' ++ non_shared	

solve_internal :: Board -> [(String,Strategy)] -> Maybe ([String], Board)
solve_internal raw_board named_strategies =		
	apply_strategies (initial_propagate raw_board) []
	where
		initial_propagate :: Board -> Board
		initial_propagate board = foldl propagate_constraints board (filter is_distinct board)
		apply_strategies_once :: Board -> [(String,Strategy)] -> Maybe(String,Board)
		apply_strategies_once board [] = Nothing
		apply_strategies_once board ((name,strategy):rest) =
			case strategy board of
				[] -> apply_strategies_once board rest
				cells -> 
					let board' = foldl (\b c -> propagate_constraints (replace_by_position b c) c) board cells in
					Just (name, board')
		apply_strategies :: Board -> [String] -> Maybe([String],Board)
		apply_strategies board names = 
			case apply_strategies_once board named_strategies of
				Just (name, board') -> 
					let names' = names ++ [name] in
					if is_complete board' then Just (names', board') else apply_strategies board' names'
				_ -> trace (board2string board False) Nothing
		replace_by_position :: Board -> Cell -> Board
		replace_by_position board c@(p,_) = c : filter (\(p1,_) -> p1 /= p) board

solve  :: Board -> Maybe ([String], Board)
solve board = solve_internal board strategies
	where strategies = [("only_choice",only_choice),
						("only_square", only_square),
						("two_out_of_three", two_out_of_three),
						("naked_single", naked_single)]

-----------------------------------------------------------------------------
-- test
-----------------------------------------------------------------------------

is_valid_solution :: Board -> Bool
is_valid_solution board =
	let
		groupBy f xs = toList $ fromListWith (++) [(k', [v]) | (k, v) <- xs, let k' = f k ] 
		cols = groupBy (\(c,_,_)->c) board
		rows = groupBy (\(_,r,_)->r) board
		blks = groupBy (\(_,_,b)->b) board
	in
		all is_distinct board &&
		length cols == 9 &&
		length rows == 9 &&
		length blks == 9 &&
		all (\(_,vs) -> intersect [1..9] (concat vs) == [1..9]) cols &&
		all (\(_,vs) -> intersect [1..9] (concat vs) == [1..9]) rows &&
		all (\(_,vs) -> intersect [1..9] (concat vs) == [1..9]) blks

is_valid_solution_ :: Maybe ([String], Board) -> Bool
is_valid_solution_ (Just (_,board)) = is_valid_solution board
is_valid_solution_ _ = False

-- puzzle:   ...7...58.56218793......1.........81...376...96.........5........4.2183.87...3...
-- solution: 123769458456218793789435162347952681518376249962184375235847916694521837871693524
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

-- puzzle:   825631974.67.24..84....76.2.59.482611.8269745.4.175.8.3.14.....5....34..294..65..
-- solution: 825631974967524138413897652759348261138269745642175389371452896586913427294786513
-- rated: very easy by http://www.sudoku-solutions.com/
-- can be solved using single possibily only
-- test ok: displayBoard $ snd $ fromJust $ solve_internal sample2 [("singlePossibility", singlePossibility)]
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
	"694521837" ++
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

-- http://www.svd.se/kultur/spel/sudoku/
sampleEasy :: Board
sampleEasy = string2board $
	"26.81.3.." ++
	"5.47.9..." ++ 
	"......4.6" ++
	"..35.6148" ++
	".4....927" ++
	"8...97..." ++
	"1.6..28.4" ++
	".89.4.712" ++
	"47..3..5."

run_test = 
	let 
		are_equal a b = a \\ b == [] && b \\ a == []
	in
		not . any (==False) $ 
			[
			are_equal (only_choice sample1) [((0,1,0),[4])],	
			are_equal (only_square sample3) [((2,8,6),[1]),((2,0,0),[3])],
			are_equal (naked_single sample2)
				[((0,1,0),[9]), 
				 ((2,2,0),[3]), 
				 ((0,3,3),[7]), 
				 ((3,3,4),[3]), 
				 ((1,4,3),[3]), 
				 ((0,5,3),[6]), 
				 ((6,5,5),[3]), 
				 ((5,6,7),[2]), 
				 ((6,6,8),[8]), 
				 ((2,7,6),[6])],
			are_equal (two_out_of_three sample4)
				[((8,1,2),[1]), 
				 ((6,0,2),[3]), 
				 ((5,0,1),[4]), 
				 ((2,3,3),[1]), 
				 ((2,4,3),[3]),
				 ((6,5,5),[4]),
				 ((6,4,5),[8]),
				 ((3,8,7),[2]),
				 ((1,6,6),[9]),
				 ((1,0,0),[7]),
				 ((5,7,7),[5]),
				 ((3,1,1),[7]),
				 ((3,4,4),[9]),
				 ((7,8,8),[5]),
				 ((8,3,5),[9])],
			--is_valid_solution_ (solve sample1), -- could not solve
			is_valid_solution_ (solve sample2),
			is_valid_solution_ (solve sample3),
			is_valid_solution_ (solve sample4),
			is_valid_solution_ (solve sampleEasy)
			]
