%%%%%%%%%%%%%   INCLUDES   %%%%%%%%%%%%`

:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].


%%%%%%%%%%%%%%%%%%%     FIND ALL PINS AND SKEWERS ON THE BOARD      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chessboard_pins_skewers(State, Pins, Skewers):-
        identify_all_pins_and_skewers(State, 0, Pins_Dirty, Skewers_Dirty),
        clean_pins(Pins_Dirty, Pins),
	clean_pins(Skewers_Dirty, Skewers).

clean_pins([], []).

clean_pins([H|T],  Pins):-
        length(H, 0),
        !,
        clean_pins(T, Pins).

clean_pins([H|T],[H|Pins]):-
        clean_pins(T, Pins).

identify_all_pins_and_skewers(_,8,[],[]):- !.

identify_all_pins_and_skewers(State, Rank, [Pins_on_this_rank | OtherPins], [Skewers_on_this_rank | OtherSkewers]):-
        find_all_pins_and_skewers_rank(State, Rank, 0, Pins_on_this_rank, Skewers_on_this_rank),
        NewRank is Rank + 1,
        identify_all_pins_and_skewers(State, NewRank, OtherPins, OtherSkewers).

find_all_pins_and_skewers_rank(_, _, 8, [], []):- !.

find_all_pins_and_skewers_rank(State, Rank, File, Pins, Skewers):-
        identify_piece([Rank, File], State, Piece),
        identify_pin_skewer(Piece, Rank, File,State, Pins_Empty, Skewers_Empty),
        length(Pins_Empty,0),
	length(Skewers_Empty,0),
        !,
        NewFile is File + 1,
        find_all_pins_and_skewers_rank(State, Rank, NewFile, Pins, Skewers).

find_all_pins_and_skewers_rank(State, Rank, File, [Pins_entry | OtherPins_on_this_rank], Skewers):-
        identify_piece([Rank, File], State, Piece),
        identify_pin_skewer(Piece, Rank, File, State, Pins_by_this_piece, Skewers_Empty),
	length(Skewers_Empty, 0), 
	!,
        index_to_square([Rank, File],Square),
        nth0(0, Pins_entry, Square, Pins_by_this_piece),
        NewFile is File + 1,
        find_all_pins_and_skewers_rank(State, Rank, NewFile, OtherPins_on_this_rank, Skewers).

find_all_pins_and_skewers_rank(State, Rank, File, Pins, [Skewers_entry | OtherSkewers_on_this_rank]):-
	identify_piece([Rank, File], State, Piece),
        identify_pin_skewer(Piece, Rank, File, State, Pins_Empty, Skewers_by_this_piece),
	length(Pins_Empty, 0),
	!, 
	index_to_square([Rank, File],Square),
        nth0(0, Skewers_entry, Square, Skewers_by_this_piece),
        NewFile is File + 1,
        find_all_pins_and_skewers_rank(State, Rank, NewFile, Pins, OtherSkewers_on_this_rank).

find_all_pins_and_skewers_rank(State, Rank, File, [Pins_entry | OtherPins_on_this_rank], [Skewers_entry | OtherSkewers_on_this_rank]):-
        identify_piece([Rank, File], State, Piece),
        identify_pin_skewer(Piece, Rank, File, State, Pins_by_this_piece, Skewers_by_this_piece),
        index_to_square([Rank, File],Square),
	nth0(0, Pins_entry, Square, Pins_by_this_piece),
        nth0(0, Skewers_entry, Square, Skewers_by_this_piece),
        NewFile is File + 1,
        find_all_pins_and_skewers_rank(State, Rank, NewFile, OtherPins_on_this_rank, OtherSkewers_on_this_rank).




%%%%%%%%%%%%%%%%%%%%%%     THE PIN     %%%%%%%%%%%%%%%%%%%%%%%%%%%
identify_pin_skewer(' ',_,_,_,[],[]):- !.
identify_pin_skewer('p',_,_,_,[],[]):- !.
identify_pin_skewer('P',_,_,_,[],[]):- !.
identify_pin_skewer('k',_,_,_,[],[]):- !.
identify_pin_skewer('K',_,_,_,[],[]):- !.
identify_pin_skewer('n',_,_,_,[],[]):- !.
identify_pin_skewer('N',_,_,_,[],[]):- !.

identify_pin_skewer(Piece, Rank, File, State, Pins, Skewers):-
        colour(Piece, Colour),
        piece_real_control_squares(Piece, Rank, File, State, CtrlSquares),
        attacked_pieces(Colour, State, CtrlSquares, AttackedPieces),
        find_pinned_pieces(Piece, Rank, File, State, AttackedPieces, Pins, Skewers).

find_pinned_pieces('Q', Rank, File, State, AllAttacked, Pins, Skewers):-
        seperate_possible0(AllAttacked, Rank, File, DiagonalAttacked, HorVertAttacked),
        find_pinned_pieces('R', Rank, File, State, HorVertAttacked, HorVertPins, HorVertSkewers),
        find_pinned_pieces('B', Rank, File, State, DiagonalAttacked, DiagonalPins, DiagonalSkewers),
        append(HorVertPins, DiagonalPins, Pins),
	append(HorVertSkewers, DiagonalSkewers, Skewers).

find_pinned_pieces('q', Rank, File, State, AllAttacked, Pins, Skewers):-
        seperate_possible(AllAttacked, Rank, File, DiagonalAttacked, HorVertAttacked),
        find_pinned_pieces('r', Rank, File, State, HorVertAttacked, HorVertPins, HorVertSkewers),
        find_pinned_pieces('b', Rank, File, State, DiagonalAttacked, DiagonalPins, DiagonalSkewers),
        append(HorVertPins, DiagonalPins, Pins),
	append(HorVertSkewers, DiagonalSkewers, Skewers).

find_pinned_pieces('R', _, _, _, [], [], []):- !.

find_pinned_pieces('r', _, _, _, [], [], []):- !.

find_pinned_pieces('R', Rank, File, State, [H|T], [[AttackedPiece, 'k', absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, 'k'),!,
        find_pinned_pieces('R', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('R', Rank, File, State, [H|T], Pins, [['k', SkeweredTo, absoluteSkewer] | Tail]):-
	nth0(0, H, 'k', AttackedIndex), !, 
	find_relative_positionR(Rank, File, AttackedIndex, Direction),
	pinned_toR(AttackedIndex, Direction, State, SkeweredTo),
	find_pinned_pieces('R', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('r', Rank, File, State, [H|T], [[AttackedPiece, 'K', absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, 'K'),!,
        find_pinned_pieces('r', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('r', Rank, File, State, [H|T], Pins, [['K', SkeweredTo, absoluteSkewer]|Tail]):-
	nth0(0, H, 'K', AttackedIndex), !,
	find_relative_positionR(Rank, File, AttackedIndex, Direction),
	pinned_toR(AttackedIndex, Direction, State, SkeweredTo),
	find_pinned_pieces('r', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('R', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, black),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('R', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('R', Rank, File, State, [H|T], Pins, [[AttackedPiece, SkeweredTo, relativeSkewer]|Tail]):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, SkeweredTo),
        colour(SkeweredTo, black),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(SkeweredTo, SkeweredToEval),
        SkeweredToEval < AttackedEval, !,
        find_pinned_pieces('R', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('R', Rank, File, State, [_|T], Pins, Skewers):-
        find_pinned_pieces('R', Rank, File, State, T, Pins, Skewers).

find_pinned_pieces('r', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, white),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('r', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('r', Rank, File, State, [H|T], Pins, [[AttackedPiece, SkeweredTo, relativeSkewer]|Tail]):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, SkeweredTo),
        colour(SkeweredTo, white),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(SkeweredTo, SkeweredToEval),
        SkeweredToEval < AttackedEval, !,
        find_pinned_pieces('r', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('r', Rank, File, State, [_|T], Pins, Skewers):-
        find_pinned_pieces('r', Rank, File, State, T, Pins, Skewers).

find_pinned_pieces('B', _, _, _, [], [], []):- !.

find_pinned_pieces('b', _, _, _, [], [], []):- !.

find_pinned_pieces('B', Rank, File, State, [H|T], [[AttackedPiece, 'k', absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, 'k'), !,
        find_pinned_pieces('B', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('B', Rank, File, State, [H|T], Pins, [['k', SkeweredTo, absoluteSkewer]|Tail]):-
        nth0(0, H, 'k', AttackedIndex), !,
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, SkeweredTo), 
        find_pinned_pieces('B', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('b', Rank, File, State, [H|T], [[AttackedPiece, 'K', absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, 'K'), !,
        find_pinned_pieces('b', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('b', Rank, File, State, [H|T], Pins, [['K', SkeweredTo, absoluteSkewer]|Tail]):-
        nth0(0, H, 'K', AttackedIndex), !,
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, SkeweredTo), 
        find_pinned_pieces('b', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('B', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, black),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('B', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('B', Rank, File, State, [H|T], Pins, [[AttackedPiece, SkeweredTo, relativeSkewer]|Tail]):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, SkeweredTo),
        colour(SkeweredTo, black),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(SkeweredTo, SkeweredToEval),
        SkeweredToEval < AttackedEval, !,
        find_pinned_pieces('B', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('B', Rank, File, State, [_|T], Pins, Skewers):-
        find_pinned_pieces('B', Rank, File, State, T, Pins, Skewers).

find_pinned_pieces('b', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, white),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('b', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('b', Rank, File, State, [H|T], Pins, [[AttackedPiece, SkeweredTo, relativePin]|Tail]):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, SkeweredTo),
        colour(SkeweredTo, white),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(SkeweredTo, SkeweredToEval),
        SkeweredToEval < AttackedEval, !,
        find_pinned_pieces('b', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('b', Rank, File, State, [_|T], Pins, Skewers):-
        find_pinned_pieces('b', Rank, File, State, T, Pins, Skewers).

seperate_possible0([],_,_,[],[]).

seperate_possible0([H|T], Rank, File, DiagonalAttacked, [[Piece, Rank, NewFile]| Tail]):-
        nth0(0, H, Piece, [Rank, NewFile]),!,
        seperate_possible0(T, Rank, File, DiagonalAttacked, Tail).

seperate_possible0([H|T], Rank, File, DiagonalAttacked, [[Piece, NewRank, File] | Tail]):-
        nth0(0, H, Piece, [NewRank, File]),!,
        seperate_possible0(T, Rank, File, DiagonalAttacked, Tail).

seperate_possible0([H|T], Rank, File, [[Piece, NewRank, NewFile]|Tail], HorVertAttacked):-
        nth0(0, H, Piece, [NewRank, NewFile]),
        seperate_possible0(T, Rank, File, Tail, HorVertAttacked).

pinned_toB([0,_], upleft, _, ' '):- !.
pinned_toB([_,0], upleft, _, ' '):- !.

pinned_toB([Rank, File], upleft, State, PinnedTo):-
        NewRank is Rank - 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], upleft, State, PinnedTo).

pinned_toB([Rank, File], upleft, State, PinnedTo):-
        NewRank is Rank - 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile], State, PinnedTo).

pinned_toB([7,_], downleft, _, ' '):- !.
pinned_toB([_,0], downleft, _, ' '):- !.

pinned_toB([Rank, File], downleft, State, PinnedTo):-
        NewRank is Rank + 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], downleft, State, PinnedTo).

pinned_toB([Rank, File], downleft, State, PinnedTo):-
        NewRank is Rank + 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile],  State, PinnedTo).

pinned_toB([0,_], upright, _, ' '):- !.
pinned_toB([_,7], upright, _, ' '):- !.

pinned_toB([Rank, File], upright, State, PinnedTo):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], upright, State, PinnedTo).

pinned_toB([Rank, File], upright, State, PinnedTo):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, PinnedTo).

pinned_toB([7,_], downright, _, ' '):- !.
pinned_toB([_,7], downright, _, ' '):- !.

pinned_toB([Rank, File], downright, State, PinnedTo):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], downright, State, PinnedTo).

pinned_toB([Rank, File], downright, State, PinnedTo):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, PinnedTo).

pinned_toR([0,_], north, _, ' '):- !.

pinned_toR([Rank, File], north, State, PinnedTo):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], State, ' '),
        !,
        pinned_toR([NewRank, File], north, State, PinnedTo).

pinned_toR([Rank, File], north, State, PinnedTo):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], State, PinnedTo).

pinned_toR([7,_], south, _, ' '):- !.

pinned_toR([Rank, File], south, State, PinnedTo):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], State, ' '),
        !,
        pinned_toR([NewRank, File], south, State, PinnedTo).

pinned_toR([Rank, File], south, State, PinnedTo):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], State, PinnedTo).

pinned_toR([_,0], west, _, ' '):- !.

pinned_toR([Rank, File], west, State, PinnedTo):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], State, ' '),
        !,
        pinned_toR([Rank, NewFile], west, State, PinnedTo).

pinned_toR([Rank, File], west, State, PinnedTo):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], State, PinnedTo).

pinned_toR([_,7], east, _, ' '):- !.

pinned_toR([Rank, File], east, State, PinnedTo):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], State, ' '),
        !,
        pinned_toR([Rank, NewFile], east, State, PinnedTo).

pinned_toR([Rank, File], east, State, PinnedTo):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], State, PinnedTo).

