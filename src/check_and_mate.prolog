%%%%%  INCLUDES  %%%%%

:- ['./chessboard_basics.prolog'].
:- ['./attackers_and_defenders.prolog'].
:- ['./valid_moves.prolog'].
:- ['./control_squares.prolog'].

%%%%%%  CHECKMATE   %%%%%%  %%% FIX FOR EN PASSANT (DONE)%%% FIX FOR >1 ATTACKERS

identify_checkmate(ChessBoard, Next_to_Play, EnPassant, checkmate_found):-
	return_piece_colour(king, Next_to_Play, Mated_King),
	find_certain_piece(Mated_King, ChessBoard, 0, [[[King_Rank, King_File]]]),
	target_piece(Next_to_Play, King_Rank, King_File, ChessBoard, _, AttackingPiecesList),
	length(AttackingPiecesList, AttackedTimes),
	AttackedTimes > 0, 
	find_possible_king_move_squares(King_Rank, King_File, King_Possible_Square_List),
	check_possible_king_move_squares(ChessBoard, Next_to_Play, King_Possible_Square_List, Uncontrolled_Squares_List),
	Uncontrolled_Squares_List = [],	
	identify_valid_moves(ChessBoard, Next_to_Play, EnPassant, ValidMoves),
	identify_blocks_captures(AttackingPiecesList, King_Rank, King_File, ValidMoves, Blocks, Captures),
	Blocks = [],
	Captures = [], !.
	
identify_checkmate(ChessBoard, Next_to_Play, _, just_a_check):-
	return_piece_colour(king, Next_to_Play, Checked_King),
        find_certain_piece(Checked_King, ChessBoard, 0, [[[King_Rank, King_File]]]),
        target_piece(Next_to_Play, King_Rank, King_File, ChessBoard, _, AttackingPiecesList),
        length(AttackingPiecesList, AttackedTimes),
	AttackedTimes > 0, !.
	
identify_checkmate(_, _, _, keep_playing).

identify_blocks_captures(AttackingPiecesList, TargetRank, TargetFile, ValidMoves, Blocks, Captures):-
	length(AttackingPiecesList, 1), !,
	AttackingPiecesList = [Head],
	check_for_blocks_captures_on_attacker(Head, TargetRank, TargetFile, ValidMoves, Blocks, Captures).
	


check_for_blocks_captures([], _, _,  _, [], []):- !.

check_for_blocks_captures([[Piece, Rank, File]|T], TargetRank, TargetFile, Valid_Moves, [Block_on_attacker|RestBlocks], [Capture_on_attacker|RestCaptures]):-
	check_for_blocks_captures_on_attacker([Piece, Rank, File], TargetRank, TargetFile, Valid_Moves, Block_on_attacker, Capture_on_attacker),
	check_for_blocks_captures(T, TargetRank, TargetFile, Valid_Moves, RestBlocks, RestCaptures).


check_for_blocks_captures_on_attacker(['P', AttRank, AttFile], _, _, Valid_Moves, [], Fillers):-
	!, fillable_square(Valid_Moves, AttRank, AttFile, Fillers).

check_for_blocks_captures_on_attacker(['p', AttRank, AttFile], _, _, Valid_Moves, [], Fillers):-
	!, fillable_square(Valid_Moves, AttRank, AttFile, Fillers).

check_for_blocks_captures_on_attacker(['N', AttRank, AttFile], _, _, Valid_Moves, [], Fillers):-
        !, fillable_square(Valid_Moves, AttRank, AttFile, Fillers).

check_for_blocks_captures_on_attacker(['n', AttRank, AttFile], _, _, Valid_Moves, [], Fillers):-
        !, fillable_square(Valid_Moves, AttRank, AttFile, Fillers).

check_for_blocks_captures_on_attacker(['R', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
	!, fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
	find_relative_positionR(AttRank, AttFile, [TargetRank, TargetFile], Direction), 
	find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
	identify_blockers(Valid_Moves, IntermediateSquares, Blockers).

check_for_blocks_captures_on_attacker(['r', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
        !, fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
        find_relative_positionR(AttRank, AttFile, [TargetRank, TargetFile], Direction),
        find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
        identify_blockers(Valid_Moves, IntermediateSquares, Blockers).

check_for_blocks_captures_on_attacker(['B', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
        !, fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
        find_relative_positionB(AttRank, AttFile, [TargetRank, TargetFile], Direction),
        find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
        identify_blockers(Valid_Moves, IntermediateSquares, Blockers).

check_for_blocks_captures_on_attacker(['b', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
        !, fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
        find_relative_positionR(AttRank, AttFile, [TargetRank, TargetFile], Direction),
        find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
        identify_blockers(Valid_Moves, IntermediateSquares, Blockers).

check_for_blocks_captures_on_attacker(['Q', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
        fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
        find_relative_positionR(AttRank, AttFile, [TargetRank, TargetFile], Direction), !,
        find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
        identify_blockers(Valid_Moves, IntermediateSquares, Blockers).

check_for_blocks_captures_on_attacker(['Q', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
        fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
        find_relative_positionB(AttRank, AttFile, [TargetRank, TargetFile], Direction), !,
        find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
        identify_blockers(Valid_Moves, IntermediateSquares, Blockers).

check_for_blocks_captures_on_attacker(['q', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
        fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
        find_relative_positionR(AttRank, AttFile, [TargetRank, TargetFile], Direction), !,
        find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
        identify_blockers(Valid_Moves, IntermediateSquares, Blockers).

check_for_blocks_captures_on_attacker(['q', AttRank, AttFile], TargetRank, TargetFile, Valid_Moves, Blockers, Capturers):-
        fillable_square(Valid_Moves, AttRank, AttFile, Capturers),
        find_relative_positionB(AttRank, AttFile, [TargetRank, TargetFile], Direction), !,
        find_intermediate_squares([AttRank, AttFile], [TargetRank, TargetFile], Direction, IntermediateSquares),
        identify_blockers(Valid_Moves, IntermediateSquares, Blockers).


identify_blockers(Valid_Moves, IntermediateSquares, Blockers):-
	identify_all_blockers(Valid_Moves, IntermediateSquares, Blockers0),
	append(Blockers0, Blockers).

identify_all_blockers(_, [], []):- !.

identify_all_blockers(Valid_Moves, [[NextIntermediateRank, NextIntermediateFile]|RestIntermediate], [BlockersForThisSquare|RestBlockers]):-
	fillable_square(Valid_Moves, NextIntermediateRank, NextIntermediateFile, BlockersForThisSquare),
	identify_all_blockers(Valid_Moves, RestIntermediate, RestBlockers).
	


find_intermediate_squares([EndRankPlus, File], [EndRank, File], north, []):-
	RankBuff is EndRank + 1,
	RankBuff = EndRankPlus, !.

find_intermediate_squares([Rank, File], [EndRank, File], north, [[NewRank, File]|T]):-
	!, NewRank is Rank - 1,
	find_intermediate_squares([NewRank, File], [EndRank, File], north, T).
	
find_intermediate_squares([EndRankMinus, File], [EndRank, File], south, []):-
	RankBuff is EndRank - 1,
	RankBuff = EndRankMinus, !.

find_intermediate_squares([Rank, File], [EndRank, File], south, [[NewRank, File]|T]):-
	!, NewRank is Rank + 1,
	find_intermediate_squares([NewRank, File], [EndRank, File], south, T).

find_intermediate_squares([Rank, EndFilePlus], [Rank, EndFile], west, []):-
	FileBuff is EndFile + 1,
	FileBuff = EndFilePlus, !.

find_intermediate_squares([Rank, File], [Rank, EndFile], west, [[Rank, NewFile]|T]):-
	!, NewFile is File - 1,
	find_intermediate_squares([Rank, NewFile], [Rank, EndFile], west, T).

find_intermediate_squares([Rank, EndFileMinus], [Rank, EndFile], east, []):-
	FileBuff is EndFile - 1,
	FileBuff = EndFileMinus, !.

find_intermediate_squares([Rank, File], [Rank, EndFile], east, [[Rank, NewFile]|T]):-
	!, NewFile is File + 1,
	find_intermediate_squares([Rank, NewFile], [Rank, EndFile], east, T).

find_intermediate_squares([EndRankPlus, EndFilePlus], [EndRank, EndFile], upleft, []):-
	RankBuff is EndRank + 1,
	FileBuff is EndFile + 1,
	RankBuff = EndRankPlus, 
	FileBuff = EndFilePlus, !.

find_intermediate_squares([Rank, File], [EndRank, EndFile], upleft, [[NewRank, NewFile]|T]):-
	!, NewRank is Rank - 1,
	NewFile is File - 1,
	find_intermediate_squares([NewRank, NewFile], [EndRank, EndFile], upleft, T).

find_intermediate_squares([EndRankPlus, EndFileMinus], [EndRank, EndFile], upright, []):-
	RankBuff is EndRank + 1, 
	FileBuff is EndFile - 1,
	RankBuff = EndRankPlus,
	FileBuff = EndFileMinus, !.

find_intermediate_squares([Rank, File], [EndRank, EndFile], upright, [[NewRank, NewFile]|T]):-
	!, NewRank is Rank - 1,
	NewFile is File + 1,
	find_intermediate_squares([NewRank, NewFile], [EndRank, EndFile], upright, T).

find_intermediate_squares([EndRankMinus, EndFilePlus], [EndRank, EndFile], downleft, []):-
	RankBuff is EndRank - 1, 
	FileBuff is EndFile + 1, 
	RankBuff = EndRankMinus,
	FileBuff = EndFilePlus, !.

find_intermediate_squares([Rank, File], [EndRank, EndFile], downleft, [[NewRank, NewFile]|T]):-
	!, NewRank is Rank + 1,
	NewFile is File - 1, 
	find_intermediate_squares([NewRank, NewFile], [EndRank, EndFile], downleft, T).

find_intermediate_squares([EndRankMinus, EndFileMinus], [EndRank, EndFile], downright, []):-
	RankBuff is EndRank - 1,
	FileBuff is EndFile - 1,
	RankBuff = EndRankMinus, 
	FileBuff = EndFileMinus, !.

find_intermediate_squares([Rank, File], [EndRank, EndFile], downright, [[NewRank, NewFile]|T]):-
	!, NewRank is Rank + 1,
	NewFile is File + 1,
	find_intermediate_squares([NewRank, NewFile], [EndRank, EndFile], downright, T).

check_possible_king_move_squares(_, _, [], []):- !.

check_possible_king_move_squares(ChessBoard, Next_to_Play, [[Rank, File]|T], Uncontrolled_Squares_List):-
	identify_piece([Rank, File], ChessBoard, Piece),
	Piece \= ' ', !,
	check_possible_king_move_squares(ChessBoard, Next_to_Play, T, Uncontrolled_Squares_List).

check_possible_king_move_squares(ChessBoard, Next_to_Play, [[Rank, File]|T], Uncontrolled_Squares_List):-
	target_piece(Next_to_Play, Rank, File, ChessBoard,  _, [_|_]), !,
	check_possible_king_move_squares(ChessBoard, Next_to_Play, T, Uncontrolled_Squares_List).

check_possible_king_move_squares(ChessBoard, Next_to_Play, [[Rank, File]|T], [[Rank, File]|Tail]):-
	check_possible_king_move_squares(ChessBoard, Next_to_Play, T, Tail).

find_possible_king_move_squares(0, 0, [[0,1],[1,1],[1,0]]):- !.
find_possible_king_move_squares(7, 0, [[6,0],[6,1],[7,1]]):- !.
find_possible_king_move_squares(0, 7, [[0,6],[1,6],[1,7]]):- !.
find_possible_king_move_squares(7, 7, [[6,7],[6,6],[7,6]]):- !.

find_possible_king_move_squares(0, File, [[0, FileMinus], [0, FilePlus], [1, FileMinus], [1, File], [1, FilePlus]]):-
	!, FileMinus is File - 1,
	FilePlus is File + 1.

find_possible_king_move_squares(7, File, [[7, FileMinus], [7, FilePlus], [6, FileMinus], [6, File], [6, FilePlus]]):-
	!, FileMinus is File - 1,
	FilePlus is File + 1.

find_possible_king_move_squares(Rank, 0, [[RankMinus, 0], [RankPlus, 0], [RankMinus, 1], [RankPlus, 1], [Rank, 1]]):-
	!, RankMinus is Rank - 1,
	RankPlus is Rank + 1.

find_possible_king_move_squares(Rank, 7, [[RankMinus, 7], [RankPlus, 7], [RankMinus, 6], [RankPlus, 6], [Rank, 6]]):-
	!, RankMinus is Rank - 1,
	RankPlus is Rank + 1.

find_possible_king_move_squares(Rank, File, [[RankMinus, FileMinus], [RankMinus, File], [RankMinus, FilePlus], [Rank, FileMinus], [Rank, FilePlus], [RankPlus, FileMinus], [RankPlus, File], [RankPlus, FilePlus]]):-
	RankMinus is Rank - 1,
	RankPlus is Rank + 1,
	FileMinus is File - 1,
	FilePlus is File + 1.
