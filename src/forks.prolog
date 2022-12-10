%%%%%%%    INCLUDES      %%%%%%%

:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].

%%%%%%%%%%    FORKS     %%%%%%%%%% 

%identify_forks(+ChessBoard, -Forks)
% 
identify_forks(ChessBoard, Forks):-
	identify_chessboard_forks(0, ChessBoard, Forks0),
	fix_forks(Forks0, Forks).

identify_chessboard_forks(8, _, []):- !.

identify_chessboard_forks(Rank, ChessBoard, [Forks_on_this_rank|OtherForks]):-
	identify_forks_rank(Rank, 0, ChessBoard, Forks_on_this_rank),
	NewRank is Rank + 1,
	identify_chessboard_forks(NewRank, ChessBoard, OtherForks).

identify_forks_rank(_, 8, _, []):- !.

identify_forks_rank(Rank, File, ChessBoard, [Forking_piece|OtherForks]):-
	identify_forking_piece(Rank, File, ChessBoard, Forking_piece),  
	NewFile is File + 1, 
	identify_forks_rank(Rank, NewFile, ChessBoard, OtherForks).

identify_forking_piece(Rank, File, ChessBoard, []):-
	identify_piece([Rank, File], ChessBoard, ' '), !.

identify_forking_piece(Rank, File, ChessBoard, Forks):-
	identify_piece([Rank, File], ChessBoard, Piece),
	colour(Piece, Colour),
	piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquares),
	attacked_pieces(Colour, ChessBoard, CtrlSquares, AttackedPieces), 
	reverse_colour(Colour, DefColour),
	identify_threat_on_piece_list(AttackedPieces, ChessBoard, DefColour, ThreatenedPieces), 
	length(ThreatenedPieces, NoOfThreatenedPieces),
	NoOfThreatenedPieces > 1, !, 
	forked_pieces(ThreatenedPieces, Forks0),
	index_to_square([Rank, File], Square),
	append([[Square], [Piece], [NoOfThreatenedPieces]], Info),
	append(Info, Forks0, Forks).

identify_forking_piece(_, _, _, []).

identify_threat_on_piece_list([], _, _, []):- !.

identify_threat_on_piece_list([[Piece, Rank, File]|RestAttacked], ChessBoard, Colour, [[Piece, Rank, File]|Tail]):-
        identify_threat_on_piece(Rank, File, ChessBoard, Colour, Threats),
        Threats = [_, _, Score],
        Score = check, !,
        identify_threat_on_piece_list(RestAttacked, ChessBoard, Colour, Tail).

identify_threat_on_piece_list([[Piece, Rank, File]|RestAttacked], ChessBoard, Colour, [[Piece, Rank, File]|Tail]):-
	identify_threat_on_piece(Rank, File, ChessBoard, Colour, Threats),
	Threats = [_, _, Score], 
	Score > 0, !,
	identify_threat_on_piece_list(RestAttacked, ChessBoard, Colour, Tail).

identify_threat_on_piece_list([_|RestAttacked], ChessBoard, Colour, ThreatenedPieces):-
	identify_threat_on_piece_list(RestAttacked, ChessBoard, Colour, ThreatenedPieces).

fix_forks([],[]).

fix_forks([H|T], Forks):-
	fix_forks_rank(H, []), !,
	fix_forks(T, Forks).

fix_forks([H|T], [Forks_on_this_rank|OtherForks]):-
	fix_forks_rank(H, Forks_on_this_rank),
	fix_forks(T, OtherForks).

fix_forks_rank([],[]).

fix_forks_rank([[]|T], Forks):-
	!, fix_forks_rank(T, Forks).

fix_forks_rank([H|T],[H|OtherForks]):-
	fix_forks_rank(T, OtherForks).


forked_pieces([],[]).

forked_pieces([[Piece, Rank, File]|OtherAttacked], [[Square, Piece]|Tail]):-
	index_to_square([Rank, File], Square),
	forked_pieces(OtherAttacked, Tail).
