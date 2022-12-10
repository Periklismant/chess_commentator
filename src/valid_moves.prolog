%%%%%%%   INCLUDES    %%%%%%%%

:- ['chessboard_basics.prolog'].
:- ['control_squares.prolog'].
:- ['attackers_and_defenders.prolog'].

%%%%%%%%%%%%%%%%%%%

:- discontiguous piece_move_squares/6.

%%%%%%%%   ALL VALID MOVES   %%%%%%%%%%

identify_valid_moves(ChessBoard, Colour, EnPassant, ValidMoves):-
	identify_all_valid_moves(ChessBoard, 0, Colour, EnPassant, ValidMoves0), 
	append(ValidMoves0, ValidMoves).

identify_all_valid_moves(_, 8, _, _, []):- !.

identify_all_valid_moves(ChessBoard, Rank, Colour, EnPassant, ValidMoves):-
	identify_all_valid_moves_rank(ChessBoard, Rank, 0, Colour, EnPassant, ValidMovesRank),
	ValidMovesRank = [], !,
	RankPlus is Rank + 1, 
	identify_all_valid_moves(ChessBoard, RankPlus, Colour, EnPassant, ValidMoves).

identify_all_valid_moves(ChessBoard, Rank, Colour, EnPassant, [ValidMovesRank|RestValidMoves]):-
	identify_all_valid_moves_rank(ChessBoard, Rank, 0, Colour, EnPassant, ValidMovesRank),
	RankPlus is Rank + 1,
	identify_all_valid_moves(ChessBoard, RankPlus, Colour, EnPassant, RestValidMoves).

identify_all_valid_moves_rank(_, _, 8, _, _, []):- !.

identify_all_valid_moves_rank(ChessBoard, Rank, File, Colour, EnPassant, [ValidMovesPiece|RestValidMoves]):-
	identify_piece([Rank, File], ChessBoard, Piece),
	colour(Piece, Colour), !,
	piece_move_squares(Piece, Rank, File, EnPassant, ChessBoard, ValidMovesPiece0),
	append([Piece, Rank, File], [ValidMovesPiece0], ValidMovesPiece), 
	FilePlus is File + 1, 
	identify_all_valid_moves_rank(ChessBoard, Rank, FilePlus, Colour, EnPassant, RestValidMoves).

identify_all_valid_moves_rank(ChessBoard, Rank, File, Colour, EnPassant, ValidMoves):-
	FilePlus is File + 1,
	identify_all_valid_moves_rank(ChessBoard, Rank, FilePlus, Colour, EnPassant, ValidMoves).

%%%%%%%%   POSSIBLE MOVE SQUARES FOR EACH PIECE   %%%%%%%%%%%%

piece_move_squares('P', 6, 0, _, ChessBoard, ValidMoves):-
	!, check_pawn_move_squares([[5, 0], [4, 0]], ChessBoard, MoveSquares),
	check_pawn_capture_squares([[5,1]], white, [], ChessBoard, CaptureSquares),
	append(MoveSquares, CaptureSquares, ValidMoves), !.

piece_move_squares('P', 6, 7, _, ChessBoard, ValidMoves):- 
        !, check_pawn_move_squares([[5, 7], [4, 7]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[5,6]], white, [], ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves), !.

piece_move_squares('P', 6, File, _, ChessBoard, ValidMoves):-
	!, FileMinus is File - 1,
	FilePlus is File + 1, 
        check_pawn_move_squares([[5, File], [4, File]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[5, FileMinus], [5, FilePlus]], white, [], ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves), !.

piece_move_squares('P', Rank, 0, EnPassant, ChessBoard, ValidMoves):-
	!, RankMinus is Rank - 1,
	check_pawn_move_squares([[RankMinus, 0]], ChessBoard, MoveSquares),
	check_pawn_capture_squares([[RankMinus, 1]], white, EnPassant, ChessBoard, CaptureSquares),
	append(MoveSquares, CaptureSquares, ValidMoves).

piece_move_squares('P', Rank, 7, EnPassant, ChessBoard, ValidMoves):-
        !, RankMinus is Rank - 1,
        check_pawn_move_squares([[RankMinus, 7]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[RankMinus, 6]], white, EnPassant, ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves).

piece_move_squares('P', Rank, File, EnPassant, ChessBoard, ValidMoves):-
	!, RankMinus is Rank - 1,
	FileMinus is File - 1,
	FilePlus is File + 1,
	check_pawn_move_squares([[RankMinus, File]], ChessBoard, MoveSquares),
	check_pawn_capture_squares([[RankMinus, FileMinus], [RankMinus, FilePlus]], white, EnPassant, ChessBoard, CaptureSquares),
	append(MoveSquares, CaptureSquares, ValidMoves).

piece_move_squares('p', 1, 0, _, ChessBoard, ValidMoves):-
        !, check_pawn_move_squares([[2, 0], [3, 0]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[2,1]], black, [], ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves), !.

piece_move_squares('p', 1, 7, _, ChessBoard, ValidMoves):-
        !, check_pawn_move_squares([[2, 7], [3, 7]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[2,6]], black, [], ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves), !.

piece_move_squares('p', 1, File, _, ChessBoard, ValidMoves):-
        !, FileMinus is File - 1,
        FilePlus is File + 1,
        check_pawn_move_squares([[2, File], [3, File]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[2, FileMinus], [2, FilePlus]], black, [], ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves), !.

piece_move_squares('p', Rank, 0, EnPassant, ChessBoard, ValidMoves):-
        !, RankPlus is Rank + 1,
        check_pawn_move_squares([[RankPlus, 0]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[RankPlus, 1]], black, EnPassant, ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves).

piece_move_squares('p', Rank, 7, EnPassant, ChessBoard, ValidMoves):-
        !, RankPlus is Rank + 1,
        check_pawn_move_squares([[RankPlus, 7]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[RankPlus, 6]], black, EnPassant, ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves).

piece_move_squares('p', Rank, File, EnPassant, ChessBoard, ValidMoves):-
        !, RankPlus is Rank + 1,
        FileMinus is File - 1,
        FilePlus is File + 1,
        check_pawn_move_squares([[RankPlus, File]], ChessBoard, MoveSquares),
        check_pawn_capture_squares([[RankPlus, FileMinus], [RankPlus, FilePlus]], black, EnPassant, ChessBoard, CaptureSquares),
        append(MoveSquares, CaptureSquares, ValidMoves).

check_pawn_move_squares([], _, []):- !.  %%% Suppose one-square-move first in input list.

check_pawn_move_squares([[Rank, File]|T], ChessBoard, [[Rank, File]|Tail]):-
	identify_piece([Rank, File], ChessBoard, ' '), !,
	check_pawn_move_squares(T, ChessBoard, Tail).

check_pawn_move_squares(_, _, []).

check_pawn_capture_squares([], _, _, _, []):- !.

check_pawn_capture_squares([[Rank, File]|T], Colour, [Rank, File], ChessBoard, [[Rank, File]|Tail]):-
	!, check_pawn_capture_squares(T, Colour, [Rank, File], ChessBoard, Tail).

check_pawn_capture_squares([[Rank, File]|T], Colour, EnPassant, ChessBoard, [[Rank, File]|Tail]):-
	identify_piece([Rank, File], ChessBoard, Piece),
	colour(Piece, MyColour),
	reverse_colour(Colour, MyColour), !, 
	check_pawn_capture_squares(T, Colour, EnPassant, ChessBoard, Tail).

check_pawn_capture_squares([_|T], Colour, EnPassant, ChessBoard, ValidMoves):-
	check_pawn_capture_squares(T, Colour, EnPassant, ChessBoard, ValidMoves).

piece_move_squares('K', Rank, File, _, ChessBoard, ValidMoves):-
	piece_real_control_squares('K', Rank, File, ChessBoard, CtrlSquares),
	filter_squares_of_colour_pieces(white, ChessBoard, CtrlSquares, WhitePiecesSquares),
	subtract(CtrlSquares, WhitePiecesSquares, ValidMoves0),
	squares_without_threats(ValidMoves0, ChessBoard, white, ValidMoves), !.

piece_move_squares('k', Rank, File, _, ChessBoard, ValidMoves):-
	piece_real_control_squares('k', Rank, File, ChessBoard, CtrlSquares),
	filter_squares_of_colour_pieces(black, ChessBoard, CtrlSquares, BlackPiecesSquares),
        subtract(CtrlSquares, BlackPiecesSquares, ValidMoves0),
        squares_without_threats(ValidMoves0, ChessBoard, black, ValidMoves), !.



piece_move_squares(Piece, Rank, File, _, ChessBoard, ValidMoves):- 
	piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquares),
	colour(Piece, Colour),
	filter_squares_of_colour_pieces(Colour, ChessBoard, CtrlSquares, WhitePiecesSquares),
	subtract(CtrlSquares, WhitePiecesSquares, ValidMoves).

squares_without_threats([], _, _, []):- !.

squares_without_threats([[Rank, File]|T], ChessBoard, Colour, [[Rank,File]|Tail]):-
	target_piece(Colour, Rank, File, ChessBoard, _, Attackers), 
	length(Attackers, 0), !,
	squares_without_threats(T, ChessBoard, Colour, Tail).

squares_without_threats([_|T], ChessBoard, Colour, ValidMoves):-
	squares_without_threats(T, ChessBoard, Colour, ValidMoves).

filter_squares_of_colour_pieces(_, _, [], []):- !.

filter_squares_of_colour_pieces(Colour, ChessBoard, [[Rank, File]|T], [[Rank, File]|Tail]):-
	identify_piece([Rank, File], ChessBoard, Piece),
	colour(Piece, Colour), !,
	filter_squares_of_colour_pieces(Colour, ChessBoard, T, Tail).

filter_squares_of_colour_pieces(Colour, ChessBoard, [_|T], ColourPiecesSquares):-
	filter_squares_of_colour_pieces(Colour, ChessBoard, T, ColourPiecesSquares).

colour_or_enpassant([Rank, File], _, [Rank, File]):- !.

colour_or_enpassant(ChessBoard, [Rank, File], Colour, _):-
	identify_piece([Rank, File], ChessBoard, TargetPiece),
	colour(TargetPiece, Colour).


fillable_square([], _, _, []):- !.

fillable_square([[Piece, PieceRank, PieceFile, PieceValidMoves]|T], Rank, File, [[Piece, PieceRank, PieceFile]|Tail]):-
	member([Rank, File], PieceValidMoves), !,
	fillable_square(T, Rank, File, Tail).

fillable_square([_|T], Rank, File, Fillers):-
	fillable_square(T, Rank, File, Fillers).
