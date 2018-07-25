%%%%%%%%%%%%%   INCLUDES   %%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].


%%%%%%%%%%%%%%%%%%%     FIND ALL PINS ON THE BOARD      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chessboard_pins(State, Pins):-
        identify_all_pins(State, 0, Pins_Dirty),
        clean_pins(Pins_Dirty, Pins).

clean_pins([], []).

clean_pins([H|T],  Pins):-
        length(H, 0),
        !,
        clean_pins(T, Pins).

clean_pins([H|T],[H|Pins]):-
        clean_pins(T, Pins).

identify_all_pins(_,8,[]):- !.

identify_all_pins(State, Rank, [Pins_on_this_rank | OtherPins]):-
        find_all_pins_rank(State, Rank, 0, Pins_on_this_rank),
        NewRank is Rank + 1,
        identify_all_pins(State, NewRank, OtherPins).

find_all_pins_rank(_, _, 8, []):- !.

find_all_pins_rank(State, Rank, File, Pins):-
        identify_piece([Rank, File], State, Piece),
        identify_pin(Piece, Rank, File,State, Pins_Empty),
        length(Pins_Empty,0),
        !,
        NewFile is File + 1,
        find_all_pins_rank(State, Rank, NewFile, Pins).

find_all_pins_rank(State, Rank, File, [Pins_entry  | OtherPins_on_this_rank]):-
        identify_piece([Rank, File], State, Piece),
        identify_pin(Piece, Rank, File, State, Pins_by_this_piece),
        index_to_square([Rank, File],Square),
        nth0(0, Pins_entry, Square, Pins_by_this_piece),
        NewFile is File + 1,
        find_all_pins_rank(State, Rank, NewFile, OtherPins_on_this_rank).

%%%%%%%%%%%%%%%%%%%%%%     THE PIN     %%%%%%%%%%%%%%%%%%%%%%%%%%%
identify_pin(' ',_,_,_,[]):- !.
identify_pin('p',_,_,_,[]):- !.
identify_pin('P',_,_,_,[]):- !.
identify_pin('k',_,_,_,[]):- !.
identify_pin('K',_,_,_,[]):- !.
identify_pin('n',_,_,_,[]):- !.
identify_pin('N',_,_,_,[]):- !.

identify_pin(Piece, Rank, File, State, Pins):-
        colour(Piece, Colour),
        piece_control_squares(Piece, Rank, File, AllPossibleCtrlSquares),
        remove_blocked_squares(Piece, Rank, File, State, AllPossibleCtrlSquares, BlockedList, []),
        subtract(AllPossibleCtrlSquares, BlockedList, CtrlSquares),
        attacked_pieces(Colour, State, CtrlSquares, AttackedPieces),
        find_pinned_pieces(Piece, Rank, File, State, AttackedPieces, Pins).

find_pinned_pieces('Q', Rank, File, State, AllAttacked, Pins):-
        seperate_possible0(AllAttacked, Rank, File, DiagonalAttacked, HorVertAttacked),
        find_pinned_pieces('R', Rank, File, State, HorVertAttacked, HorVertPins),
        find_pinned_pieces('B', Rank, File, State, DiagonalAttacked, DiagonalPins),
        append(HorVertPins, DiagonalPins, Pins).

find_pinned_pieces('q', Rank, File, State, AllAttacked, Pins):-
        seperate_possible(AllAttacked, Rank, File, DiagonalAttacked, HorVertAttacked),
        find_pinned_pieces('r', Rank, File, State, HorVertAttacked, HorVertPins),
        find_pinned_pieces('b', Rank, File, State, DiagonalAttacked, DiagonalPins),
        append(HorVertPins, DiagonalPins, Pins).

find_pinned_pieces('R', _, _, _, [], []):- !.

find_pinned_pieces('r', _, _, _, [], []):- !.

find_pinned_pieces('R', Rank, File, State, [H|T], [[AttackedPiece, 'k', absolutePin]|Tail] ):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, 'k'),!,
        find_pinned_pieces('R', Rank, File, State, T, Tail).

find_pinned_pieces('r', Rank, File, State, [H|T], [[AttackedPiece, 'K', absolutePin]|Tail] ):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, 'K'),!,
        find_pinned_pieces('r', Rank, File, State, T, Tail).

find_pinned_pieces('R', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail] ):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, black),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('R', Rank, File, State, T, Tail).

find_pinned_pieces('R', Rank, File, State, [_|T], Pins):-
        find_pinned_pieces('R', Rank, File, State, T, Pins).

find_pinned_pieces('r', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail] ):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionR(Rank, File, AttackedIndex, Direction),
        pinned_toR(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, white),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('r', Rank, File, State, T, Tail).

find_pinned_pieces('r', Rank, File, State, [_|T], Pins):-
        find_pinned_pieces('r', Rank, File, State, T, Pins).

find_pinned_pieces('B', _, _, _, [], []):- !.

find_pinned_pieces('b', _, _, _, [], []):- !.

find_pinned_pieces('B', Rank, File, State, [H|T], [[AttackedPiece, 'k', absolutePin]|Tail]):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, 'k'), !,
        find_pinned_pieces('B', Rank, File, State, T, Tail).

find_pinned_pieces('b', Rank, File, State, [H|T], [[AttackedPiece, 'K', absolutePin]|Tail]):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, 'K'), !,
        find_pinned_pieces('b', Rank, File, State, T, Tail).

find_pinned_pieces('B', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail] ):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, black),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('B', Rank, File, State, T, Tail).

find_pinned_pieces('B', Rank, File, State, [_|T], Pins):-
        find_pinned_pieces('B', Rank, File, State, T, Pins).

find_pinned_pieces('b', Rank, File, State, [H|T], [[AttackedPiece, PinnedTo, relativePin]|Tail] ):-
        nth0(0, H, AttackedPiece, AttackedIndex),
        find_relative_positionB(Rank, File, AttackedIndex, Direction),
        pinned_toB(AttackedIndex, Direction, State, PinnedTo),
        colour(PinnedTo, white),
        evaluate(AttackedPiece, AttackedEval),
        evaluate(PinnedTo, PinnedToEval),
        PinnedToEval > AttackedEval, !,
        find_pinned_pieces('b', Rank, File, State, T, Tail).

find_pinned_pieces('b', Rank, File, State, [_|T], Pins):-
        find_pinned_pieces('b', Rank, File, State, T, Pins).

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

