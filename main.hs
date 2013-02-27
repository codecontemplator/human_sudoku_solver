-- http://www.sudokudragon.com/sudokustrategy.htm
-- http://www.sudoku-solutions.com/

-----------------------------------------------------------------------------
-- imports
-----------------------------------------------------------------------------

import Data.List
import Data.Char(digitToInt, intToDigit)
import Data.Maybe(fromJust, isJust, isNothing, listToMaybe)
import Debug.Trace(trace)
import Data.List.Split(chunksOf)
import Data.Map(fromListWith, toList)
import Control.Exception(assert)
import Control.Monad.State

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
is_group_member (Col c1) (c,_,_) = c1 == c
is_group_member (Row r1) (_,r,_) = r1 == r
is_group_member (Block b1) (_,_,b) = b1 == b

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
-- Naked pair
--
-- The "naked pair" solving technique is an intermediate solving technique. In this technique the 
-- Sudoku is scanned for a pair of cells in a row, column or box containing only the same two candidates. 
-- Since these candidates must go in these cells, they can therefore be removed from the candidate lists 
-- of all other unsolved cells in that row, column or box. Reducing candidate lists may reveal a hidden 
-- or naked single in another unsolved cell, generally however the technique is a step to solving the 
-- next cell. 
--
-- ref: http://www.sudoku-solutions.com/solvingNakedSubsets.php#nakedPair
--
naked_pair :: Strategy
naked_pair board = concat $
	[ reduced |
		g <- all_groups,
		let cells_in_g = [ c | c@(p,_) <- board, is_group_member g p ],
		c1@(p1,vs1) <- cells_in_g, c2@(p2,vs2) <- cells_in_g, p1 < p2,
		length vs1 == 2, vs1 == vs2,
		let reducable = (filter (not.is_distinct) cells_in_g) \\ [c1, c2],
		length reducable > 0,
		let reduced = map (\(p,vs)->(p,vs\\vs1)) reducable
	]

--
-- Hidden single 
--
-- The hidden single solving technique is a very effective but still simple solving 
-- technique. Using this technique the candidate values of all empty cells in a given 
-- row, column and box are determined. If a given candidate value appears in only one 
-- cell in a row, column or box then that must be the value of the cell. 
--
-- ref: http://www.sudoku-solutions.com/solvingHiddenSubsets.php#hiddenSingle
--
hidden_single :: Strategy
hidden_single board = 
	let 
		full_info = 
			[ ((head hits,[v]),(g,empty_cells_in_g,hits,[ c | c@(p,_) <- board, is_group_member g p, is_distinct c])) | 
				g <- all_groups,
				let (filled_cells_in_g, empty_cells_in_g) = partition is_distinct [ c | c@(p,_) <- board, is_group_member g p ],
				let selected_vs = map cell_value filled_cells_in_g,
				v <- [1..9],
				let hits = [ p | c@(p,vs) <- empty_cells_in_g, elem v vs, not (elem v selected_vs)],
				length hits == 1
			]
	in
		trace ("hidden single, full info " ++ show full_info) (map fst full_info)

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

type SolveState = (Board,[String],Board)
type StrategyDef = (String,Strategy)

state_to_board :: SolveState -> Board
state_to_board (board,_,_) = board

state_to_actions :: SolveState -> [String]
state_to_actions (_, actions, _) = actions

state_to_solution :: SolveState -> Board
state_to_solution (_, _, solution) = solution

modify_board :: (Board -> Board) -> State SolveState ()
modify_board f = modify $ \(board,actions,solution) -> (f board, actions, solution)

modify_actions :: ([String] -> [String]) -> State SolveState ()
modify_actions f = modify $ \(board,actions,solution) -> (board, f actions, solution)

propagate_constraints :: Board -> Cell -> Board
propagate_constraints board c@(p,v) = 
	let
		(shared, non_shared) = partition (is_shared p . cell_position) board
		shared' = map (\(p1,vs) -> (p1, vs \\ v)) shared
	in
		if length v == 1 then 
			shared' ++ non_shared
		else 
			board

propagate_all_constraints :: Board -> Board
propagate_all_constraints board = foldl propagate_constraints board (filter is_distinct board)

brute_force_solve :: Board -> [Board]
brute_force_solve board =
	let 
		board' = propagate_all_constraints board
		least_ambiguous_cell@(p,vs) = head $ sortBy (\(_,vs1) (_,vs2)->compare (length vs1) (length vs2)) (filter (not.is_distinct) board')
		candidates = map (\v -> (p,[v])) vs
		update_cell b c = c : filter (\(p1,_) -> p1 /= p) b
		boards = map (update_cell board') candidates
	in
		if is_complete board' then
			[board']
		else
			concatMap brute_force_solve boards

is_valid_board :: Board -> Maybe Board -> Bool
is_valid_board board maybe_solution = length board == 81 && null duplicates && test_candiates
	where 
		duplicates = 
			[ c | 
				g <- all_groups, 
				let cells_in_g = [ c | c@(p,_)<-board, is_distinct c, is_group_member g p],
				c@(p,[v]) <- cells_in_g, c'@(p',[v']) <- cells_in_g,
				p /= p', v == v'
			]
		test_candiates = case maybe_solution of 
							Just solution -> 
								let 
									find_cell_value :: Position -> Int
									find_cell_value p = head $ snd $ fromJust $ find (\(p1,_)->p==p1) solution
									is_valid_candidate :: Cell -> Bool
									is_valid_candidate (p,vs) = elem (find_cell_value p) vs
								in
									all is_valid_candidate board
							Nothing -> True

validate_state :: State SolveState ()
validate_state = do
	board <- gets state_to_board
	solution <- gets state_to_solution
	if is_valid_board board (Just solution) then
		return ()
	else
		error "State is invalid."

apply_strategy :: StrategyDef -> State SolveState [Cell]
apply_strategy (_,strategy) = gets (strategy.state_to_board)

update_cell :: Cell -> State SolveState ()
update_cell c@(p,_) = modify_board $ \board -> c : filter (\(p1,_) -> p1 /= p) board

propagate_constraintsM :: Cell -> State SolveState ()
propagate_constraintsM cell = modify_board $ \board -> propagate_constraints board cell

update_solution :: [Cell] -> State SolveState ()
update_solution solved_cells =
	forM_ solved_cells $ \cell -> do --(propagate_constraintsM >> update_cell)
		board <- gets state_to_board
		trace ("before update:" ++ show cell ++ ";\n" ++ (board2string board False)) validate_state
		update_cell cell
		board' <- gets state_to_board
		trace ("after cell update:" ++ show cell ++ ";\n" ++ (board2string board' False)) validate_state
		propagate_constraintsM cell
		board'' <- gets state_to_board
		trace ("after constraint propagation:" ++ show cell ++ ";\n" ++ (board2string board'' False)) validate_state
						
record_action :: StrategyDef -> [Cell] -> State SolveState ()
record_action (name,_) solved_cells = do
		board <- gets state_to_board
		modify_actions $ \actions -> actions++[name]

solveM :: [StrategyDef] -> State SolveState Bool
solveM all_strategies =
	let
		solve' [] = return False		
		solve' (strategy:strategies) = do
			solved <- gets (is_complete.state_to_board)
			if solved then
				return True
			else do
				solved_cells <- apply_strategy strategy				
				if null solved_cells then
					solve' strategies
				else do
					trace ("strategy " ++ fst strategy ++ " applied successfully. cells=" ++ show solved_cells) record_action strategy solved_cells
					update_solution solved_cells
					solve' all_strategies
	in
		solve' all_strategies

solveM2 :: [StrategyDef] -> State SolveState (Bool, Board, [String])
solveM2 all_strategies = do
	solved <- solveM all_strategies
	(board,actions,_) <- get
	return (solved,board,actions)

solve  :: Board -> (Bool, Board, [String])
solve board = evalState (solveM2 strategies) (propagate_all_constraints board,[],solution)
	where 
		strategies = 
			[("only_choice",only_choice), 
			 ("only_square", only_square), 
			 ("two_out_of_three", two_out_of_three), 
			 ("naked_single", naked_single), 
			 ("hidden_single", hidden_single), 
			 ("naked_pair", naked_pair)]
		solution = case brute_force_solve board of { [x] -> x; _ -> error "board is not well defined"; }

-----------------------------------------------------------------------------
-- test
-----------------------------------------------------------------------------

is_valid_solution :: Board -> Bool
is_valid_solution board = is_valid_board board Nothing && is_complete board

{-
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
-}

is_valid_solution_ :: (Bool, Board, [String]) -> Bool
is_valid_solution_ (True, board, _) = is_valid_solution board
is_valid_solution_ _ = False

--string2sample :: String -> Board
--string2sample = propagate_all_constraints . string2board

-- puzzle:   ...7...58.56218793......1.........81...376...96.........5........4.2183.87...3...
-- solution: 123769458456218793789435162347952681518376249962184375235847916694521837871693524
sample_only_choice :: Board
sample_only_choice = string2board $ 
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
sample_naked_single :: Board
sample_naked_single = string2board $
	"825631974" ++
	".67.24..8" ++
	"4....76.2" ++
	".59.48261" ++
	"1.8269745" ++
	".4.175.8." ++
	"3.14....." ++
	"5....34.." ++
	"294..65.."

sample_only_square :: Board
sample_only_square = string2board $
	"...769458" ++
	"456218793" ++
	"7894351.." ++
	"347952681" ++
	"518376..." ++
	"962184375" ++
	"..58.7..." ++
	"694521837" ++
	"87.6.35.."

sample_two_out_of_three :: Board
sample_two_out_of_three = string2board $
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
sample_easy :: Board
sample_easy = string2board $
	"26.81.3.." ++
	"5.47.9..." ++ 
	"......4.6" ++
	"..35.6148" ++
	".4....927" ++
	"8...97..." ++
	"1.6..28.4" ++
	".89.4.712" ++
	"47..3..5."

-- http://www.sudoku-solutions.com/solvingNakedSubsets.php#nakedPair
sample_naked_pair :: Board
sample_naked_pair = propagate_all_constraints . string2board $
	"1.4.9..68" ++
	"956.18.34" ++
	"..84.6951" ++
	"51.....86" ++
	"8..6...12" ++
	"64..8..97" ++
	"781923645" ++
	"495.6.823" ++
	".6.854179"

run_test = 
	let 
		are_equal a b = a \\ b == [] && b \\ a == []
	in
		--not . any (==False) $ 
			[
			are_equal (only_choice sample_only_choice) [((0,1,0),[4])],	
			are_equal (only_square sample_only_square) [((2,8,6),[1]),((2,0,0),[3])],
			are_equal (naked_single sample_naked_single)
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
			are_equal (two_out_of_three sample_two_out_of_three)
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
			are_equal (naked_pair sample_naked_pair)
				[((2,4,3),[7,9]),
				 ((2,3,3),[7,9]),
				 ((6,5,5),[3,5]),
				 ((6,4,5),[3,4,5]),
				 ((6,3,5),[3,4])],
			is_valid_solution_ (solve sample_only_choice),
			is_valid_solution_ (solve sample_only_square),
			is_valid_solution_ (solve sample_naked_single),
			is_valid_solution_ (solve sample_two_out_of_three),
			is_valid_solution_ (solve sample_naked_pair),
			is_valid_solution_ (solve sample_easy)
			]

-- solve sample_only_choice -> produced invalid board and hangs