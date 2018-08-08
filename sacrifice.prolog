%%%%%%%%%%%%   INCLIUDES   %%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].
:- ['./attackers_and_defenders.prolog'].

%%%%%%%%%%%%   THE SACRIFICE   %%%%%%%%%%%%%%%

identify_sacrifice(State, NewState, Piece, [NewRank, NewFile], []):-
	is_sacrifice(State, NewState, Piece, [NewRank, NewFile], sacrifice_false), !.

identify_sacrifice(State, NewState, Piece, [NewRank, NewFile], Sacrifice):-
	is_sacrifice(State, NewState, Piece, [NewRank, NewFile, sacrifice_true).
	%%%% MORE %%%%%	



is_sacrifice(State, NewState, Piece, [NewRank, NewFile], sacrifice_true):-
	colour(Piece, Colour),
	identify_piece([NewRank, NewFile], State, CapturedPiece),
	evaluate(CapturedPiece, Gain),
	identify_threat_on_piece(NewRank, NewFile, NewState, Colour, [NewRank, NewFile, Loss]),
	Loss > Gain,
	!.

is_sacrifice(_, _, _, _, sacrifice_false).

