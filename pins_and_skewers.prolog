%%%%%%%%%%%%%   INCLUDES   %%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].
:- ['./attackers_and_defenders.prolog'].

%%%%%%%%%%%%%%%%%%%     ACTUAL PINS SKEWERS     %%%%%%%%%%%%%%%%%%%%%%%%

actual_pins_skewers(_, [],[]):- !.

actual_pins_skewers(ChessBoard, [[_, Pins_by_this_Piece]|RestPins], ActualPins):-
	actual_pins_skewers_piece(ChessBoard, Pins_by_this_Piece, Empty),
	Empty = [],  !,
	actual_pins_skewers(ChessBoard, RestPins, ActualPins).

actual_pins_skewers(ChessBoard, [[Square, Pins_by_this_Piece]|RestPins], [[Square, Actual_pins_by_this_piece]|RestActualPins]):-
	actual_pins_skewers_piece(ChessBoard, Pins_by_this_Piece, Actual_pins_by_this_piece),
	actual_pins_skewers(ChessBoard, RestPins, RestActualPins).  

actual_pins_skewers_piece(_, [], []):- !.

actual_pins_skewers_piece(ChessBoard, [[AttackedInfo, PinnedInfo, absolutePin]|RestPins], [[AttackedInfo, PinnedInfo, absolutePin, illegal_to_move]|ActualPins]):-
	!, actual_pins_skewers_piece(ChessBoard, RestPins, ActualPins).

%No absolute skewer needed
	
actual_pins_skewers_piece(ChessBoard, [[[_AttackedPiece, AttackedRank, AttackedFile], [PinnedToPiece, PinnedToRank, PinnedToFile], _Relative]|RestPins], ActualPins):-
	remove_add_piece(ChessBoard, AttackedRank, AttackedFile, ' ', NoAttackedState),
	colour(PinnedToPiece, Colour),
	identify_threat_on_piece(PinnedToRank, PinnedToFile, NoAttackedState, Colour, [PinnedToRank, PinnedToFile, NoGains]),
	NoGains =< 0, !,
	actual_pins_skewers_piece(ChessBoard, RestPins, ActualPins). 

actual_pins_skewers_piece(ChessBoard, [[[AttackedPiece, AttackedRank, AttackedFile], [PinnedToPiece, PinnedToRank, PinnedToFile], Relative]|RestPins], [[[AttackedPiece, AttackedSquare], [PinnedToPiece, PinnedToSquare], Relative, Gains]|RestActualPins]):-
	remove_add_piece(ChessBoard, AttackedRank, AttackedFile, ' ', NoAttackedState),
	colour(PinnedToPiece, Colour),
	identify_threat_on_piece(PinnedToRank, PinnedToFile, NoAttackedState, Colour, [PinnedToRank, PinnedToFile, Gains]),
	index_to_square([AttackedRank, AttackedFile], AttackedSquare),
	index_to_square([PinnedToRank, PinnedToFile], PinnedToSquare),
	actual_pins_skewers_piece(ChessBoard, RestPins, RestActualPins). 	

%%%%%%%%%%%%%%%%%%%     FIND ALL PINS AND SKEWERS ON THE BOARD      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chessboard_pins_skewers(State, Pins_Actual, Skewers_Actual):-
        identify_all_pins_and_skewers(State, 0, Pins_Dirty, Skewers_Dirty),
        clean_pins(Pins_Dirty, Pins),
	clean_pins(Skewers_Dirty, Skewers),
	actual_pins_skewers(State, Pins, Pins_Actual),
	actual_pins_skewers(State, Skewers, Skewers_Actual).

clean_pins([Rank0, Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7], Pins):-
	append([Rank0, Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7], Pins).
		
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
	append([Square], [Pins_by_this_piece], Pins_entry),
        NewFile is File + 1,
        find_all_pins_and_skewers_rank(State, Rank, NewFile, OtherPins_on_this_rank, Skewers).

find_all_pins_and_skewers_rank(State, Rank, File, Pins, [Skewers_entry | OtherSkewers_on_this_rank]):-
	identify_piece([Rank, File], State, Piece),
        identify_pin_skewer(Piece, Rank, File, State, Pins_Empty, Skewers_by_this_piece),
	length(Pins_Empty, 0),
	!, 
	index_to_square([Rank, File],Square),
        append([Square], [Skewers_by_this_piece], Skewers_entry),
        NewFile is File + 1,
        find_all_pins_and_skewers_rank(State, Rank, NewFile, Pins, OtherSkewers_on_this_rank).

find_all_pins_and_skewers_rank(State, Rank, File, [Pins_entry | OtherPins_on_this_rank], [Skewers_entry | OtherSkewers_on_this_rank]):-
        identify_piece([Rank, File], State, Piece),
        identify_pin_skewer(Piece, Rank, File, State, Pins_by_this_piece, Skewers_by_this_piece),
        index_to_square([Rank, File],Square),
	append([Square], [Pins_by_this_piece], Pins_entry),
        append([Square], [Skewers_by_this_piece], Skewers_entry),
	NewFile is File + 1,
        find_all_pins_and_skewers_rank(State, Rank, NewFile, OtherPins_on_this_rank, OtherSkewers_on_this_rank).


%%%%%%%%%%%%%%%%%%%%%%     THE PIN / THE SKEWER     %%%%%%%%%%%%%%%%%%%%%%%%%%%
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
        seperate_possible0(AllAttacked, Rank, File, DiagonalAttacked, HorVertAttacked),
        find_pinned_pieces('r', Rank, File, State, HorVertAttacked, HorVertPins, HorVertSkewers),
        find_pinned_pieces('b', Rank, File, State, DiagonalAttacked, DiagonalPins, DiagonalSkewers),
        append(HorVertPins, DiagonalPins, Pins),
	append(HorVertSkewers, DiagonalSkewers, Skewers).

find_pinned_pieces('R', _, _, _, [], [], []):- !.

find_pinned_pieces('r', _, _, _, [], [], []):- !.

find_pinned_pieces('R', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], ['k', PinnedToRank, PinnedToFile] , absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toR([AttackedRank, AttackedFile], Direction, State, 'k', PinnedToRank, PinnedToFile), !,
        find_pinned_pieces('R', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('R', Rank, File, State, [H|T], Pins, [[['k', AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], absoluteSkewer] | Tail]):-
	nth0(0, H, 'k', [AttackedRank, AttackedFile]), !, 
	find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
	pinned_toR([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile),
	find_pinned_pieces('R', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('r', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], ['K', PinnedToRank, PinnedToFile], absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toR([AttackedRank, AttackedFile], Direction, State, 'K', PinnedToRank, PinnedToFile),!,
        find_pinned_pieces('r', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('r', Rank, File, State, [H|T], Pins, [[['K', AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], absoluteSkewer]|Tail]):-
	nth0(0, H, 'K', [AttackedRank, AttackedFile]), !,
	find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
	pinned_toR([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile),
	find_pinned_pieces('r', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('R', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], [PinnedTo, PinnedToRank, PinnedToFile], relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toR([AttackedRank, AttackedFile], Direction, State, PinnedTo, PinnedToRank, PinnedToFile),
        colour(PinnedTo, black),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(PinnedTo, PinnedToEval),
       % PinnedToEval > AttackedEval, 
	!,
        find_pinned_pieces('R', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('R', Rank, File, State, [H|T], Pins, [[[AttackedPiece, AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], relativeSkewer]|Tail]):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toR([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile),
        colour(SkeweredTo, black),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(SkeweredTo, SkeweredToEval),
       % SkeweredToEval < AttackedEval, 
	!,
        find_pinned_pieces('R', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('R', Rank, File, State, [_|T], Pins, Skewers):-
        find_pinned_pieces('R', Rank, File, State, T, Pins, Skewers).

find_pinned_pieces('r', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], [PinnedTo, PinnedToRank, PinnedToFile], relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toR([AttackedRank, AttackedFile], Direction, State, PinnedTo, PinnedToRank, PinnedToFile),
        colour(PinnedTo, white),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(PinnedTo, PinnedToEval),
       % PinnedToEval > AttackedEval, 
	!,
        find_pinned_pieces('r', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('r', Rank, File, State, [H|T], Pins, [[[AttackedPiece, AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], relativeSkewer]|Tail]):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionR(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toR([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile),
        colour(SkeweredTo, white),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(SkeweredTo, SkeweredToEval),
       % SkeweredToEval < AttackedEval, 
	!,
        find_pinned_pieces('r', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('r', Rank, File, State, [_|T], Pins, Skewers):-
        find_pinned_pieces('r', Rank, File, State, T, Pins, Skewers).

find_pinned_pieces('B', _, _, _, [], [], []):- !.

find_pinned_pieces('b', _, _, _, [], [], []):- !.

find_pinned_pieces('B', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], ['k', PinnedToRank, PinnedToFile], absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, 'k', PinnedToRank, PinnedToFile), !,
        find_pinned_pieces('B', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('B', Rank, File, State, [H|T], Pins, [[['k', AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], absoluteSkewer]|Tail]):-
        nth0(0, H, 'k', [AttackedRank, AttackedFile]), !,
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile), 
        find_pinned_pieces('B', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('b', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], ['K', PinnedToRank, PinnedToFile], absolutePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, 'K', PinnedToRank, PinnedToFile), !,
        find_pinned_pieces('b', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('b', Rank, File, State, [H|T], Pins, [[['K', AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], absoluteSkewer]|Tail]):-
        nth0(0, H, 'K', [AttackedRank, AttackedFile]), !,
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile), 
        find_pinned_pieces('b', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('B', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], [PinnedTo, PinnedToRank, PinnedToFile], relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, PinnedTo, PinnedToRank, PinnedToFile),
        colour(PinnedTo, black),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(PinnedTo, PinnedToEval),
       % PinnedToEval > AttackedEval, 
	!,
        find_pinned_pieces('B', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('B', Rank, File, State, [H|T], Pins, [[[AttackedPiece, AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], relativeSkewer]|Tail]):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile),
        colour(SkeweredTo, black),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(SkeweredTo, SkeweredToEval),
       % SkeweredToEval < AttackedEval, 
	!,
        find_pinned_pieces('B', Rank, File, State, T, Pins, Tail).

find_pinned_pieces('B', Rank, File, State, [_|T], Pins, Skewers):-
        find_pinned_pieces('B', Rank, File, State, T, Pins, Skewers).

find_pinned_pieces('b', Rank, File, State, [H|T], [[[AttackedPiece, AttackedRank, AttackedFile], [PinnedTo, PinnedToRank, PinnedToFile], relativePin]|Tail], Skewers):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, PinnedTo, PinnedToRank, PinnedToFile),
        colour(PinnedTo, white),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(PinnedTo, PinnedToEval),
       % PinnedToEval > AttackedEval, 
	!,
        find_pinned_pieces('b', Rank, File, State, T, Tail, Skewers).

find_pinned_pieces('b', Rank, File, State, [H|T], Pins, [[[AttackedPiece, AttackedRank, AttackedFile], [SkeweredTo, SkeweredToRank, SkeweredToFile], relativePin]|Tail]):-
        nth0(0, H, AttackedPiece, [AttackedRank, AttackedFile]),
        find_relative_positionB(Rank, File, [AttackedRank, AttackedFile], Direction),
        pinned_toB([AttackedRank, AttackedFile], Direction, State, SkeweredTo, SkeweredToRank, SkeweredToFile),
        colour(SkeweredTo, white),
       % evaluate(AttackedPiece, AttackedEval),
       % evaluate(SkeweredTo, SkeweredToEval),  
       % SkeweredToEval < AttackedEval,
	!,
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

pinned_toB([0,_], upleft, _, ' ', ' ', ' '):- !.
pinned_toB([_,0], upleft, _, ' ', ' ', ' '):- !.

pinned_toB([Rank, File], upleft, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewRank is Rank - 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], upleft, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toB([Rank, File], upleft, State, PinnedTo, NewRank, NewFile):-
        NewRank is Rank - 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile], State, PinnedTo).

pinned_toB([7,_], downleft, _, ' ', ' ', ' '):- !.
pinned_toB([_,0], downleft, _, ' ', ' ', ' '):- !.

pinned_toB([Rank, File], downleft, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewRank is Rank + 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], downleft, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toB([Rank, File], downleft, State, PinnedTo, NewRank, NewFile):-
        NewRank is Rank + 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile],  State, PinnedTo).

pinned_toB([0,_], upright, _, ' ', ' ', ' '):- !.
pinned_toB([_,7], upright, _, ' ', ' ', ' '):- !.

pinned_toB([Rank, File], upright, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], upright, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toB([Rank, File], upright, State, PinnedTo, NewRank, NewFile):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, PinnedTo).

pinned_toB([7,_], downright, _, ' ', ' ', ' '):- !.
pinned_toB([_,7], downright, _, ' ', ' ', ' '):- !.

pinned_toB([Rank, File], downright, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, ' '),
        !,
        pinned_toB([NewRank, NewFile], downright, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toB([Rank, File], downright, State, PinnedTo, NewRank, NewFile):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], State, PinnedTo).

pinned_toR([0,_], north, _, ' ', ' ', ' '):- !.

pinned_toR([Rank, File], north, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], State, ' '),
        !,
        pinned_toR([NewRank, File], north, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toR([Rank, File], north, State, PinnedTo, NewRank, File):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], State, PinnedTo).

pinned_toR([7,_], south, _, ' ', ' ', ' '):- !.

pinned_toR([Rank, File], south, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], State, ' '),
        !,
        pinned_toR([NewRank, File], south, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toR([Rank, File], south, State, PinnedTo, NewRank, File):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], State, PinnedTo).

pinned_toR([_,0], west, _, ' ', ' ', ' '):- !.

pinned_toR([Rank, File], west, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], State, ' '),
        !,
        pinned_toR([Rank, NewFile], west, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toR([Rank, File], west, State, PinnedTo, Rank, NewFile):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], State, PinnedTo).

pinned_toR([_,7], east, _, ' ', ' ', ' '):- !.

pinned_toR([Rank, File], east, State, PinnedTo, PinnedToRank, PinnedToFile):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], State, ' '),
        !,
        pinned_toR([Rank, NewFile], east, State, PinnedTo, PinnedToRank, PinnedToFile).

pinned_toR([Rank, File], east, State, PinnedTo, Rank, NewFile):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], State, PinnedTo).

