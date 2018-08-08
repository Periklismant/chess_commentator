%%%%%%%%%%   INCLUDES   %%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].


%%%%%%%%%%%  DISCOVERED ATTACK  %%%%%%%%%%%%%%%
identify_discovered_attacks(State, NewState, OldRank, OldFile, DiscoveredAttacks):-
	identify_piece([OldRank, OldFile], State, Piece),
	colour(Piece, Colour),
	find_possible_discoveries(State, OldRank, OldFile, Colour, PossibleDiscoveries),
	find_attacked_pieces_by_list(State, PossibleDiscoveries, PreviousAttacked),
	find_attacked_pieces_by_list(NewState, PossibleDiscoveries, NowAttacked),
	find_attacked_list_difference(NowAttacked, PreviousAttacked, DiscoveredAttacks0),	
	fix_discovered_attacks(DiscoveredAttacks0, DiscoveredAttacks).

fix_discovered_attacks([],[]).

fix_discovered_attacks([[_, _, _, []]|T], DiscoveredAttacks):-
	!, fix_discovered_attacks(T, DiscoveredAttacks).

fix_discovered_attacks([H|T], [H|Tail]):-
	fix_discovered_attacks(T, Tail).	
	
find_attacked_list_difference([],[],[]):- !.
	
find_attacked_list_difference([[Piece, Rank, File, []]|TailNow], [_|TailPrev], [[Piece, Rank, File, []]|Tail]):-
	!, find_attacked_list_difference(TailNow, TailPrev, Tail).

find_attacked_list_difference([[Piece, Rank, File, AttackedNow]|TailNow], [[Piece, Rank, File, AttackedPrev]|TailPrev], [[Piece, Rank, File, DiscoveredAttackedPieces]|Tail]):-
	subtract(AttackedNow, AttackedPrev, DiscoveredAttackedPieces),
	find_attacked_list_difference(TailNow, TailPrev, Tail).

find_attacked_pieces_by_list(_, [], []):- !.

find_attacked_pieces_by_list(State, [[Piece, Rank, File]|T], [[Piece, Rank, File, AttackedList]|Tail]):-
	colour(Piece, Colour),
	piece_real_control_squares(Piece, Rank, File, State, CtrlSquares),
	attacked_pieces(Colour, State, CtrlSquares, AttackedList),
	find_attacked_pieces_by_list(State, T, Tail).
 
find_possible_discoveries(State, Rank, File, Colour, PossibleDiscoveries):-
	same_diagonal_as([Rank, File], AllSameDiagonal),
	filter_diagonal(Rank, File, State, Colour, AllSameDiagonal, PossibleDiagonal),
	same_line_as([Rank, File], AllSameLine),
	filter_line(Rank, File, State, Colour, AllSameLine, PossibleLine),
	append([PossibleDiagonal, PossibleLine], PossibleDiscoveries).	

filter_line(_, _, _, _, [], []):- !.

filter_line(TargetRank, TargetFile, ChessBoard, Colour, [[Rank, File]|T], [[Piece, Rank, File]|Tail]):-
	identify_piece([Rank, File], ChessBoard, Piece), 
	line_piece(Piece),
	colour(Piece, Colour),
	piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquares),
	member([TargetRank, TargetFile], CtrlSquares), !,
	filter_line(TargetRank, TargetFile, ChessBoard, Colour, T, Tail).

filter_line(TargetRank, TargetFile, ChessBoard, Colour, [_|T], PossibleLine):-
	filter_line(TargetRank, TargetFile, ChessBoard, Colour, T, PossibleLine).

filter_diagonal(_, _, _, _, [], []):- !.

filter_diagonal(TargetRank, TargetFile, ChessBoard, Colour, [[Rank, File]|T], [[Piece, Rank, File]|Tail]):-
	identify_piece([Rank, File], ChessBoard, Piece),
	diagonal_piece(Piece),
	colour(Piece, Colour), 
	piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquares),
	member([TargetRank, TargetFile], CtrlSquares), !,
	filter_diagonal(TargetRank, TargetFile, ChessBoard, Colour, T, Tail).

filter_diagonal(TargetRank, TargetFile, ChessBoard, Colour, [_|T], PossibleDiagonal):-
	filter_diagonal(TargetRank, TargetFile, ChessBoard, Colour, T, PossibleDiagonal).


