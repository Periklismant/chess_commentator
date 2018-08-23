%%%%%%%%%%%%    INCLUDES   %%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].

%%%%%%%%%%%%   WEAK PAWNS   %%%%%%%%%%%%%%

find_pawn_islands(ChessBoard, WhiteIslandsNo, BlackIslandsNo, WhiteIslands, BlackIslands):-
	find_all_pawns(ChessBoard, 0, WhitePawns0, BlackPawns0),
	remove_empty_list_elements(WhitePawns0, WhitePawns1),
	remove_empty_list_elements(BlackPawns0, BlackPawns1),
	append(WhitePawns1, WhitePawns2),
	append(BlackPawns1, BlackPawns2),
	sort(2, @=<, WhitePawns2, WhitePawns),
	sort(2, @=<, BlackPawns2, BlackPawns),
	group_pawns(WhitePawns, 0, WhiteIslands0),
	group_pawns(BlackPawns, 0, BlackIslands0),
	remove_empty_list_elements(WhiteIslands0, WhiteIslands1),
	remove_empty_list_elements(BlackIslands0, BlackIslands1),
	fix_islands(WhiteIslands1, WhiteIslands),
	fix_islands(BlackIslands1, BlackIslands),
	length(WhiteIslands, WhiteIslandsNo),
	length(BlackIslands, BlackIslandsNo).

fix_islands([],[]).

fix_islands([H|T], [Head|Tail]):-
	append(H, Head),
	fix_islands(T, Tail).

group_pawns([], _, []):- !.

group_pawns(Pawns, File, [Island|Rest]):-
	fill_island(Pawns, File, Next_File, RestPawns, Island),
	group_pawns(RestPawns, Next_File, Rest).

fill_island([], _, 8, [], []):- !.

fill_island([[Rank, File]|T], Id_File, NewId_File, [[Rank, File]|T], []):-
	Id_File < File, !, 
	NewId_File is Id_File + 1.

fill_island(Pawns, File, Next_File, RestPawns, [Pawns_on_File|RestIsland]):-
	pawns_on_file(Pawns, File, Pawns_on_File, PawnsLeft),
	FilePlus is File + 1,
	fill_island(PawnsLeft, FilePlus, Next_File, RestPawns, RestIsland).

pawns_on_file([], _, [], []):- !.

pawns_on_file([[Rank, File]|T], File, [[Rank, File]|T2], RestPawns):-
	!, pawns_on_file(T, File, T2, RestPawns).

pawns_on_file(Pawns, _, [], Pawns).

find_all_pawns(_, 8, [], []):- !.

find_all_pawns(ChessBoard, Rank, [WhitePawnsonRank|RestWhite], [BlackPawnsonRank|RestBlack]):-
	find_all_pawns_rank(ChessBoard, Rank, 0, WhitePawnsonRank, BlackPawnsonRank),
	NewRank is Rank + 1,
	find_all_pawns(ChessBoard, NewRank, RestWhite, RestBlack).

find_all_pawns_rank(_, _, 8, [], []):- !.

find_all_pawns_rank(ChessBoard, Rank, File, [[Rank, File]|RestWhite], BlackPawns):-
	identify_piece([Rank, File], ChessBoard, 'P'), !, 
	NewFile is File + 1,
	find_all_pawns_rank(ChessBoard, Rank, NewFile, RestWhite, BlackPawns).

find_all_pawns_rank(ChessBoard, Rank, File, WhitePawns, [[Rank, File]|RestBlack]):-
	identify_piece([Rank, File], ChessBoard, 'p'), !, 
	NewFile is File + 1,
	find_all_pawns_rank(ChessBoard, Rank, NewFile, WhitePawns, RestBlack).
	
find_all_pawns_rank(ChessBoard, Rank, File, WhitePawns, BlackPawns):-
	NewFile is File + 1,
	find_all_pawns_rank(ChessBoard, Rank, NewFile, WhitePawns, BlackPawns).

remove_empty_list_elements([],[]).

remove_empty_list_elements([[]|T], Result):-
	!, remove_empty_list_elements(T, Result).

remove_empty_list_elements([H|T], [H|Tail]):-
	remove_empty_list_elements(T, Tail).


