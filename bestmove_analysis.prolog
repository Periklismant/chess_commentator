%%%%%%%%%%%%%%%%%%%      MAIN     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move_Analysis(Stockout_File, InputFEN, Strategy, OutFile):-

	initialize_state(InputFEN, State),

	get_move_from_file(Stockout_File, BestMove, _PonderMove), %Ponder move is probably the best answer move

	string_chars(BestMove, ParsedMove), % Example: "g1f3" --> [g,'1',f,'3']

	fix_indexes(ParsedMove, FixedMove), % Example: [g,'1',f,'3'] --> [7,6,5,5] (Top left corner is [0,0]

	identify_piece(FixedMove, State, Piece,Rank, File),

	play_move(State, FixedMove, Piece,  NewState),

	states_in_file(OutFile, State, NewState),	

	nth0(2, FixedMove, NewRank),  %Get destination coordinates of Move

	nth0(3, FixedMove, NewFile),
	
%%	starting_colours(Colours0),   %% TESTING COLOURS
	
	identify_centre_control(Piece, Rank, File, NewRank, NewFile, State, NewState, ControlsTheCentre), %% Colours0, ColouringCentre),

	identify_pin(Piece, NewRank, NewFile, NewState, Pins), % ColouringPins),

        append([ControlsTheCentre, Pins], Strategy),
	
	states_in_file(OutFile, State, NewState). %%  Colouring).	

%%%%%%%%%%%%%%%%%%%%%%     THE PIN     %%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	find_pinned_pieces('R', Rank, File, State, T, Tail).
	
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
	NewRank is Rank - 1,
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

%%%%%%%%%%%%%%%%%%%%%%     CENTRE CONTROL     %%%%%%%%%%%%%%%%%%%%%%%%%%%%

identify_centre_control(Piece, Rank, File, NewRank, NewFile, State, NewState, ControlsTheCentre):- %% ColoursStart, Colouring):-	
	piece_control_squares(Piece, Rank, File, AllPossibleCtrlSquaresBefore), 
	remove_blocked_squares(Piece, Rank, File, State, AllPossibleCtrlSquaresBefore, BlockedList, []),
	subtract(AllPossibleCtrlSquaresBefore, BlockedList, CtrlSquaresBefore), %Only for Queen, Rook, Bishop
	piece_control_squares(Piece, NewRank, NewFile, AllPossibleCtrlSquaresAfter),
	remove_blocked_squares(Piece, NewRank, NewFile, NewState, AllPossibleCtrlSquaresAfter, BlockedList2, []),
	subtract(AllPossibleCtrlSquaresAfter, BlockedList2, CtrlSquaresAfter),
	%intersection(CtrlSquaresAfter, [[3,3],[3,4],[4,3],[4,4]], SquaresToColour),
	%colour_squares(SquaresToColour, 
	centre_control(CtrlSquaresBefore, CtrlSquaresAfter, ControlsTheCentre).

centre_control(Before, After, ControlsTheCentre):-
	number_of_centre_squares(Before, 0, BeforeNo),
	number_of_centre_squares(After, 0, AfterNo),
	centre_control_final_check(BeforeNo, AfterNo, ControlsTheCentre).

centre_control_final_check(BNo,ANo,Centre):-
	ANo > BNo,
	Centre = [controls_the_centre], !.

centre_control_final_check(_,_,[]).

number_of_centre_squares([], F, F).
number_of_centre_squares([H|T], Count, Final):-
	iscentresquare(H, Count, NewCount),
	number_of_centre_squares(T,NewCount,Final).

iscentresquare([3,3],Count, NewCount):-
	NewCount is Count + 1, !.

iscentresquare([3,4],Count, NewCount):-
	NewCount is Count + 1, !.

iscentresquare([4,3],Count, NewCount):-
	NewCount is Count + 1, !.
  
iscentresquare([4,4],Count, NewCount):-
	NewCount is Count + 1, !.

iscentresquare(_,Count, Count).

%%%%%%%%%%%%%%%%%%%%%%%     PLAY MOVE     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_move(State,[OldRank, OldFile, NewRank, NewFile],Piece, NewState):-
	nth0(OldRank, State, RanktoChange, BuffState),
	nth0(OldFile, RanktoChange, Piece, BuffRank),
	nth0(OldFile, ChangedRank, ' ', BuffRank),
	nth0(OldRank, ChangedState, ChangedRank, BuffState),
	nth0(NewRank, ChangedState, RanktoChange1, BuffState1),
	nth0(NewFile, RanktoChange1, _CapturedPiece, BuffRank1),
	nth0(NewFile, ChangedRank1, Piece, BuffRank1),
	nth0(NewRank, NewState, ChangedRank1, BuffState1).



%%%%%%%%%%%%%%%%%%%%%%%     READ BEST MOVE     %%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_move_from_file(File, BestMove, PonderMove):-  %FIX
	open(File, read, Str),
	readLastLine(Str,"", LastLine),
	close(Str),
	split_string(LastLine," ","", L),
	nth0(1,L,BestMove),
	nth0(3,L,PonderMove),!.

states_in_file(File, State1, State2):-
	open(File, write, Stream),
	writeState(Stream, State1),
	nl(Stream),
	nl(Stream),
	writeState(Stream, State2),
	nl(Stream),
	close(Stream), !.

writeState(_,[]).

writeState(Stream, [H|T]):-
	write(Stream, H),
	nl(Stream),
	writeState(Stream, T).	

readLine(InStream,Line):-   %Sort of Source:http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse54
         get_code(InStream,Char),
         checkCharAndReadRest(Char,Chars,InStream),
         atom_codes(Line,Chars).
   
checkCharAndReadRest(10,[],_):-  !.
     
checkCharAndReadRest(-1,[],_):-  !.
   
checkCharAndReadRest(end_of_file,[],_):-  !.
   
checkCharAndReadRest(Char,[Char|Chars],InStream):-
        get_code(InStream,NextChar),
	checkCharAndReadRest(NextChar,Chars,InStream).

readLastLine(Stream,CurrLine, LastLine):-
	at_end_of_stream(Stream), 
	LastLine = CurrLine.

readLastLine(Stream,_,LastLine):-
	\+ at_end_of_stream(Stream),
	readLine(Stream,NewCurrLine), 
	readLastLine(Stream,NewCurrLine,LastLine).



:- discontiguous piece_control_squares/4.
:- discontiguous remove_blocked_squares/7.

%%%%%%%%%%%%%%%%%%%%%%%     ATTACKED PIECES     %%%%%%%%%%%%%%%%%%%%%%%%%

attacked_pieces(_, _, [], []):- !.

attacked_pieces(Colour, State, [H|T], AttackedPieces):-
	identify_piece(H, State, ' '),
	!, 
	attacked_pieces(Colour, State, T, AttackedPieces).

attacked_pieces(white, State, [H|T], AttackedPieces):-
	identify_piece(H, State, Piece),
	colour(Piece, white),
	!,
	attacked_pieces(white, State, T, AttackedPieces).
	
attacked_pieces(black, State, [H|T], AttackedPieces):-
	identify_piece(H, State, Piece),
	colour(Piece, black), 
	!, 
	attacked_pieces(black, State, T, AttackedPieces).

attacked_pieces(Colour, State, [H|T], [AttackedPiece | Tail]):-
	identify_piece(H, State, Piece),
	append([Piece], H, AttackedPiece),
	attacked_pieces(Colour, State, T, Tail).

%%%%%%%%%%%%%%%%%%%%%%%     BLOCKED SQUARES     %%%%%%%%%%%%%%%%%%%%%%%%%

remove_blocked_squares('R',_ , _, _, [], BlockedRes,BlockedRes):- !. 

remove_blocked_squares('R', Rank, File, State, [H|T], BlockedRes, BlockedList):-
	member(H, BlockedList),
	remove_blocked_squares('R',Rank, File, State, T, BlockedRes, BlockedList), !.

remove_blocked_squares('R', Rank, File, State, [H|T], BlockedRes, BlockedList):-
	identify_piece(H, State,' '),!,
	remove_blocked_squares('R',Rank, File, State,T, BlockedRes, BlockedList), !.

remove_blocked_squares('R', Rank, File, State, [H|T], BlockedRes, BlockedList):-
	find_relative_positionR(Rank, File, H, Direction),
	fix_blocked_list(H,Direction,NewBlocked),
	append(BlockedList, NewBlocked, NewBlockedList),
	remove_blocked_squares('R', Rank, File, State, T,BlockedRes, NewBlockedList).

remove_blocked_squares('r',_ , _, _, [], BlockedRes,BlockedRes):- !.

remove_blocked_squares('r', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        member(H, BlockedList),
        remove_blocked_squares('r',Rank, File, State, T, BlockedRes, BlockedList), !.

remove_blocked_squares('r', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        identify_piece(H, State,' '),!,
        remove_blocked_squares('r',Rank, File, State,T, BlockedRes, BlockedList), !.

remove_blocked_squares('r', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        find_relative_positionR(Rank, File, H, Direction),
        fix_blocked_list(H,Direction,NewBlocked),
        append(BlockedList, NewBlocked, NewBlockedList),
        remove_blocked_squares('r', Rank, File, State, T,BlockedRes, NewBlockedList).


fix_blocked_list([0,_], north, []):- !.
fix_blocked_list([7,_], south, []):- !.
fix_blocked_list([_,0], west, []):- !.
fix_blocked_list([_,7], east, []):- !.

fix_blocked_list([0,_], upleft, []):- !.
fix_blocked_list([_,0], upleft, []):- !.

fix_blocked_list([0,_], upright, []):- !.
fix_blocked_list([_,7], upright, []):- !.

fix_blocked_list([7,_], downleft, []):- !.
fix_blocked_list([_,0], downleft, []):- !.

fix_blocked_list([7,_], downright, []):- !.
fix_blocked_list([_,7], downright, []):- !.

fix_blocked_list([Rank, File], north, [H|T]):-
	NewRank is Rank - 1, 
	H = [NewRank, File],
	fix_blocked_list([NewRank, File], north, T).
	
fix_blocked_list([Rank, File], south, [H|T]):-
	NewRank is Rank + 1, 
	H = [NewRank, File],
	fix_blocked_list([NewRank, File], south, T).

fix_blocked_list([Rank, File], west, [H|T]):-
	NewFile is File - 1, 
	H = [Rank, NewFile],
	fix_blocked_list([Rank, NewFile], west, T).

fix_blocked_list([Rank, File], east, [H|T]):-
	NewFile is File + 1, 
	H = [Rank, NewFile],
	fix_blocked_list([Rank, NewFile], east, T).

fix_blocked_list([Rank,File], upleft, [H|T]):-
	NewRank is Rank - 1,
	NewFile is File - 1,
	H = [NewRank, NewFile],
	fix_blocked_list([NewRank, NewFile], upleft, T).

fix_blocked_list([Rank,File], upright, [H|T]):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        H = [NewRank, NewFile],
        fix_blocked_list([NewRank, NewFile], upright, T).

fix_blocked_list([Rank,File], downleft, [H|T]):-
        NewRank is Rank + 1,
        NewFile is File - 1,
        H = [NewRank, NewFile],
        fix_blocked_list([NewRank, NewFile], downleft, T).

fix_blocked_list([Rank,File], downright, [H|T]):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        H = [NewRank, NewFile],
        fix_blocked_list([NewRank, NewFile], downright, T).


find_relative_positionR(Rank, _, [Target_Rank, _], north):-
	Target_Rank < Rank, !.

find_relative_positionR(Rank, _, [Target_Rank, _], south):-
	Target_Rank > Rank, !.

find_relative_positionR(_, File, [_, Target_File], east):-
	Target_File > File,!.

find_relative_positionR(_, File, [_, Target_File], west):-
	Target_File < File,!.

remove_blocked_squares('B',_ , _, _, [], BlockedRes,BlockedRes):- !.

remove_blocked_squares('B', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        member(H, BlockedList),
        remove_blocked_squares('B',Rank, File, State, T, BlockedRes, BlockedList), !.

remove_blocked_squares('B', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        identify_piece(H, State,' '),!,
        remove_blocked_squares('B',Rank, File, State,T, BlockedRes, BlockedList), !.

remove_blocked_squares('B', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        find_relative_positionB(Rank, File, H, Direction),
        fix_blocked_list(H,Direction,NewBlocked),
        append(BlockedList, NewBlocked, NewBlockedList),
        remove_blocked_squares('B', Rank, File, State, T,BlockedRes, NewBlockedList).

remove_blocked_squares('b',_ , _, _, [], BlockedRes,BlockedRes):- !.

remove_blocked_squares('b', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        member(H, BlockedList),
        remove_blocked_squares('b',Rank, File, State, T, BlockedRes, BlockedList), !.

remove_blocked_squares('b', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        identify_piece(H, State,' '),!,
        remove_blocked_squares('b',Rank, File, State,T, BlockedRes, BlockedList), !.

remove_blocked_squares('b', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        find_relative_positionB(Rank, File, H, Direction),
        fix_blocked_list(H,Direction,NewBlocked),
        append(BlockedList, NewBlocked, NewBlockedList),
        remove_blocked_squares('b', Rank, File, State, T,BlockedRes, NewBlockedList).

find_relative_positionB(Rank,File,[Target_Rank, Target_File], upleft):-
	Target_Rank < Rank,
	Target_File < File, !.

find_relative_positionB(Rank,File,[Target_Rank, Target_File], upright):-
        Target_Rank < Rank,
        Target_File > File, !.

find_relative_positionB(Rank,File,[Target_Rank, Target_File], downleft):-
        Target_Rank > Rank,
        Target_File < File, !.

find_relative_positionB(Rank,File,[Target_Rank, Target_File], downright):-
        Target_Rank > Rank,
        Target_File > File, !.

remove_blocked_squares('Q',Rank, File, State, AllPossible, BlockedRes, BlockedList):-
	seperate_possible(AllPossible, Rank, File, AllDiagonal, AllHorVert),
	remove_blocked_squares('B', Rank, File, State, AllDiagonal, BlockedRes1, BlockedList),
	remove_blocked_squares('R', Rank, File, State, AllHorVert, BlockedRes2, BlockedList),
	append(BlockedRes1, BlockedRes2, BlockedRes).

remove_blocked_squares('q',Rank, File, State, AllPossible, BlockedRes, BlockedList):-
        seperate_possible(AllPossible, Rank, File, AllDiagonal, AllHorVert),
        remove_blocked_squares('B', Rank, File, State, AllDiagonal, BlockedRes1, BlockedList),
        remove_blocked_squares('R', Rank, File, State, AllHorVert, BlockedRes2, BlockedList),
        append(BlockedRes1, BlockedRes2, BlockedRes).

seperate_possible([],_,_, [], []).

seperate_possible([[Rank, Target_File]|Tail], Rank, File, AllDiagonal, [H|HorVertTail]):-
	H = [Rank, Target_File],!, 
	seperate_possible(Tail, Rank,File, AllDiagonal, HorVertTail).

seperate_possible([[Target_Rank, File]|Tail], Rank, File, AllDiagonal, [H|HorVertTail]):-
	H = [Target_Rank, File],!,
	seperate_possible(Tail, Rank, File, AllDiagonal,HorVertTail).

seperate_possible([Head|Tail], Rank, File, [DiagonalHead|DiagonalTail], AllHorVert):-
	DiagonalHead = Head,
	seperate_possible(Tail, Rank, File, DiagonalTail, AllHorVert).
	

remove_blocked_squares(_,_, _, _, _,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%     PAWNS    %%%%%%%%%%%%%%%%%%%%%%%%	

piece_control_squares('P',Rank,0, [[CtrlRank, 1]]):-
	CtrlRank is Rank - 1, !.

piece_control_squares('P',Rank,7, [[CtrlRank, 6]]):-
	CtrlRank is Rank - 1, !.

piece_control_squares('P',Rank, File, CtrlSquares):-
	CtrlRank is Rank - 1,
	CtrlFile1 is File - 1,
	CtrlFile2 is File + 1,
	CtrlSquares = [[CtrlRank, CtrlFile1],[CtrlRank, CtrlFile2]].	

piece_control_squares('p',Rank,0, [[CtrlRank, 1]]):-
	CtrlRank is Rank + 1,!.

piece_control_squares('p',Rank,7, [[CtrlRank, 6]]):-
	CtrlRank is Rank + 1,!.

piece_control_squares('p',Rank, File, CtrlSquares):-
	CtrlRank is Rank + 1,
	CtrlFile1 is File - 1,
	CtrlFile2 is File + 1,
	CtrlSquares = [[CtrlRank, CtrlFile1],[CtrlRank, CtrlFile2]].	

%%%%%%%%%%%%%%%%%%%%%%%     KINGS     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

piece_control_squares('K', 0, 0, [[1,0],[0,1],[1,1]]):- !.

piece_control_squares('K', 0, 7, [[0,6],[1,6],[1,7]]):- !.

piece_control_squares('K', 7, 0, [[6,0],[6,1],[7,1]]):- !.

piece_control_squares('K', 7, 7, [[6,6],[7,6],[6,7]]):- !.

piece_control_squares('K',0, File, [[0,File1],[0,File2],[1,File1],[1,File],[1,File2]]):-
	File1 is File - 1,
	File2 is File + 1, !.

piece_control_squares('K',7, File, [[7,File1],[7,File2],[6,File1],[6,File],[6,File2]]):-
	File1 is File - 1,
	File2 is File + 1, !.

piece_control_squares('K',Rank, 0, [[Rank1,0],[Rank2,0],[Rank1,1],[Rank,1],[Rank2,1]]):-
	Rank1 is Rank - 1,
	Rank2 is Rank +1, !. 

piece_control_squares('K',Rank, 7, [[Rank1,7],[Rank2,7],[Rank1,6],[Rank,6],[Rank2,6]]):-
	Rank1 is Rank - 1,
	Rank2 is Rank +1, !.
 
piece_control_squares('K',Rank,File,CtrlSquares):-
	Rank1 is Rank - 1, 
	Rank2 is Rank + 1,
	File2 is File + 1,
	File1 is File - 1,
	CtrlSquares = [[Rank1,File1],[Rank1, File],[Rank1,File2],[Rank,File1],[Rank,File2],[Rank2,File1],[Rank2,File],[Rank2,File2]].

piece_control_squares('k', 0, 0, [[1,0],[0,1],[1,1]]):- !.

piece_control_squares('k', 0, 7, [[0,6],[1,6],[1,7]]):- !.

piece_control_squares('k', 7, 0, [[6,0],[6,1],[7,1]]):- !.

piece_control_squares('k', 7, 7, [[6,6],[7,6],[6,7]]):- !.

piece_control_squares('k',0, File, [[0,File1],[0,File2],[1,File1],[1,File],[1,File2]]):-
	File1 is File - 1,
	File2 is File + 1, !.

piece_control_squares('k',7, File, [[7,File1],[7,File2],[6,File1],[6,File],[6,File2]]):-
	File1 is File - 1,
	File2 is File + 1, !.

piece_control_squares('k',Rank, 0, [[Rank1,0],[Rank2,0],[Rank1,1],[Rank,1],[Rank2,1]]):-
	Rank1 is Rank - 1,
	Rank2 is Rank +1, !. 

piece_control_squares('k',Rank, 7, [[Rank1,7],[Rank2,7],[Rank1,6],[Rank,6],[Rank2,6]]):-
	Rank1 is Rank - 1,
	Rank2 is Rank +1, !.
 
piece_control_squares('k',Rank,File,CtrlSquares):-
	Rank1 is Rank - 1, 
	Rank2 is Rank + 1,
	File2 is File + 1,
	File1 is File - 1,
	CtrlSquares = [[Rank1,File1],[Rank1, File],[Rank1,File2],[Rank,File1],[Rank,File2],[Rank2,File1],[Rank2,File],[Rank2,File2]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     Rooks     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

piece_control_squares('R', Rank, File, CtrlSquares):-
	Left_SquaresNo is File,
	Right_SquaresNo is 7 - File,
	Up_SquaresNo is Rank,
	Down_SquaresNo is 7 - Rank,
	rook_left_squares(Left_SquaresNo, Rank, Left_Squares),
	rook_right_squares(Right_SquaresNo, Rank, Right_Squares),
	rook_up_squares(Up_SquaresNo, File, Up_Squares),
	rook_down_squares(Down_SquaresNo,File, Down_Squares),
	append([Left_Squares,Right_Squares,Up_Squares,Down_Squares], CtrlSquares).

piece_control_squares('r', Rank, File, CtrlSquares):-
	Left_SquaresNo is File,
	Right_SquaresNo is 7 - File,
	Up_SquaresNo is Rank,
	Down_SquaresNo is 7 - Rank,
	rook_left_squares(Left_SquaresNo, Rank, Left_Squares),
	rook_right_squares(Right_SquaresNo, Rank, Right_Squares),
	rook_up_squares(Up_SquaresNo, File, Up_Squares),
	rook_down_squares(Down_SquaresNo,File, Down_Squares),
	append([Left_Squares,Right_Squares,Up_Squares,Down_Squares], CtrlSquares).

rook_down_squares(0,_,[]):-
	!.
rook_down_squares(No, File, [H|T]):-
	NewNo is No - 1,
        Index is 7 - NewNo,
	H = [Index,File],
	rook_down_squares(NewNo, File, T).

rook_up_squares(0,_,[]):-
	!.
rook_up_squares(No,File,[H|T]):-
	NewNo is No - 1,
	H = [NewNo,File],
	rook_up_squares(NewNo, File, T).

rook_right_squares(0,_,[]):-
	!.
rook_right_squares(No,Rank,[H|T]):-
	NewNo is No - 1,
	Index is 7 - NewNo,
	H = [Rank, Index],
	rook_right_squares(NewNo, Rank, T).

rook_left_squares(0,_,[]):-
	!.
rook_left_squares(No,Rank,[H|T]):-
	NewNo is No - 1,
	H = [Rank,NewNo],
	rook_left_squares(NewNo, Rank, T).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     BISHOPS     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

piece_control_squares('B', Rank, File, CtrlSquares):-
	upleft_squares(Rank, File, Upleft_Squares),
	downright_squares(Rank, File, DownRight_Squares),
	upright_squares(Rank, File, Upright_Squares),
	downleft_squares(Rank, File, DownLeft_Squares),
	append([Upleft_Squares,DownRight_Squares,Upright_Squares, DownLeft_Squares],CtrlSquares).

piece_control_squares('b', Rank, File, CtrlSquares):-
	upleft_squares(Rank, File, Upleft_Squares),
	downright_squares(Rank, File, DownRight_Squares),
	upright_squares(Rank, File, Upright_Squares),
	downleft_squares(Rank, File, DownLeft_Squares),
	append([Upleft_Squares,DownRight_Squares,Upright_Squares, DownLeft_Squares],CtrlSquares).

downleft_squares(_,0,[]):- !.
downleft_squares(7,_,[]):- !.

downleft_squares(RankIter, FileIter, [H|T]):-
	NewRankIter is RankIter + 1,
	NewFileIter is FileIter - 1,
	H = [NewRankIter, NewFileIter],
	downleft_squares(NewRankIter, NewFileIter, T).


upright_squares(0,_,[]):- !.
upright_squares(_,7,[]):- !.

upright_squares(RankIter, FileIter, [H|T]):- 
	NewRankIter is RankIter - 1, 
	NewFileIter is FileIter + 1, 
	H = [NewRankIter, NewFileIter], 
	upright_squares(NewRankIter, NewFileIter, T).


downright_squares(7,_,[]):- !.
downright_squares(_,7,[]):- !. 

downright_squares(RankIter, FileIter,[H|T]):-
	NewRankIter is RankIter + 1, 
	NewFileIter is FileIter + 1, 
	H = [NewRankIter, NewFileIter],
	downright_squares(NewRankIter, NewFileIter, T).

upleft_squares(0,_,[]):- !.
upleft_squares(_,0,[]):- !.

upleft_squares(RankIter, FileIter,[H|T]):-
	NewRankIter is RankIter - 1, 
	NewFileIter is FileIter - 1,
	H = [NewRankIter, NewFileIter],
	upleft_squares(NewRankIter, NewFileIter, T).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     QUEENS     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

piece_control_squares('Q',Rank, File, CtrlSquares):-
	piece_control_squares('B', Rank, File, CtrlSquares1),
	piece_control_squares('R',Rank, File, CtrlSquares2 ),
	append(CtrlSquares1, CtrlSquares2, CtrlSquares). 

piece_control_squares('q',Rank, File, CtrlSquares):-
	piece_control_squares('B', Rank, File, CtrlSquares1),
	piece_control_squares('R',Rank, File, CtrlSquares2 ),
	append(CtrlSquares1, CtrlSquares2, CtrlSquares). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     KNIGHTS     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

piece_control_squares('N',Rank, File, CtrlSquares):-
	oneOClock_Square(Rank,File,OneResult),
  	twoOClock_Square(Rank,File,TwoResult),
	fourOClock_Square(Rank,File,FourResult),
	fiveOClock_Square(Rank,File,FiveResult),
	sevenOClock_Square(Rank,File,SevenResult),
	eightOClock_Square(Rank,File,EightResult),
	tenOClock_Square(Rank,File,TenResult),
	elevenOClock_Square(Rank,File,ElevenResult),
	append([OneResult,TwoResult,FourResult,FiveResult,SevenResult,EightResult,TenResult,ElevenResult], CtrlSquares).

elevenOClock_Square(0,_,[]):- !.
elevenOClock_Square(1,_,[]):- !.
elevenOClock_Square(_,0,[]):- !.
elevenOClock_Square(Rank,File, [[TargetRank, TargetFile]]):-
	TargetRank is Rank - 2,
	TargetFile is File - 1.

tenOClock_Square(0,_,[]):- !.
tenOClock_Square(_,0,[]):- !.
tenOClock_Square(_,1,[]):- !.
tenOClock_Square(Rank,File, [[TargetRank, TargetFile]]):-
	TargetRank is Rank - 1,
	TargetFile is File - 2.

eightOClock_Square(7,_,[]):- !.
eightOClock_Square(_,0,[]):- !.
eightOClock_Square(_,1,[]):- !.
eightOClock_Square(Rank,File, [[TargetRank, TargetFile]]):-
	TargetRank is Rank + 1,
	TargetFile is File - 2.

sevenOClock_Square(7,_,[]):- !.
sevenOClock_Square(6,_,[]):- !.
sevenOClock_Square(_,0,[]):- !.
sevenOClock_Square(Rank,File, [[TargetRank, TargetFile]]):-
	TargetRank is Rank + 2,
	TargetFile is File - 1.

fiveOClock_Square(7,_,[]):- !.
fiveOClock_Square(6,_,[]):- !.
fiveOClock_Square(_,7,[]):- !.
fiveOClock_Square(Rank,File, [[TargetRank, TargetFile]]):-
	TargetRank is Rank + 2,
	TargetFile is File + 1.

fourOClock_Square(7,_,[]):- !.
fourOClock_Square(_,7,[]):- !.
fourOClock_Square(_,6,[]):- !.
fourOClock_Square(Rank,File, [[TargetRank, TargetFile]]):-
	TargetRank is Rank + 1,
	TargetFile is File + 2.

twoOClock_Square(0,_,[]):- !.
twoOClock_Square(_,7,[]):- !.
twoOClock_Square(_,6,[]):- !.
twoOClock_Square(Rank,File, [[TargetRank, TargetFile]]):-
	TargetRank is Rank - 1,
	TargetFile is File + 2.

oneOClock_Square(0,_,[]):- !.
oneOClock_Square(1,_,[]):- !.
oneOClock_Square(_,7,[]):- !.
oneOClock_Square(Rank,File,[[TargetRank,TargetFile]]):-
	TargetRank is Rank - 2,
	TargetFile is File + 1. 


%%%%%%%%%%%%%%%%%%%%     ID Piece      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

identify_piece(FixedMove, State, Piece,PieceRankNo, PieceFileNo):-
	nth0(0,FixedMove, PieceRankNo), 
	nth0(PieceRankNo,State, PieceRank),
	nth0(1, FixedMove, PieceFileNo),
	nth0(PieceFileNo,PieceRank, Piece).

identify_piece(Square,State,Piece):-
	nth0(0, Square, PieceRankNo),
	nth0(1, Square, PieceFileNo),
	nth0(PieceRankNo, State, PieceRank),
	nth0(PieceFileNo,PieceRank, Piece).

fix_indexes(ParsedMove, [OldRank, OldFile, NewRank, NewFile]):-
	nth0(0, ParsedMove, FileLetter),
	file_mapping(FileLetter, OldFile),
	nth0(2, ParsedMove, FileLetter1),
	file_mapping(FileLetter1, NewFile),
	nth0(1, ParsedMove, NotationRank),
	fix_rank_index(NotationRank, OldRank),
	nth0(3, ParsedMove, NotationRank1),
	fix_rank_index(NotationRank1, NewRank).

fix_rank_index(NotationRank, StateRank):-
	atom_number(NotationRank, NotationRankInt),
	StateRank is 8 - NotationRankInt. 	

file_mapping('a',0).
file_mapping('b',1).		
file_mapping('c',2).
file_mapping('d',3).
file_mapping('e',4).
file_mapping('f',5).
file_mapping('g',6).
file_mapping('h',7).

%%%%%%%%%%%%%%%%%%%%      EVALS     %%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate('R', 5).
evaluate('r', 5).
evaluate('B', 3).
evaluate('b', 3).
evaluate('N', 3).
evaluate('n', 3).
evaluate('K', 1000).
evaluate('k', 1000).
evaluate('Q', 9).
evaluate('q', 9).
evaluate('P', 1).
evaluate('p', 1).

%%%%%%%%%%%%%%%%%%%%     COLOUR     %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% CHESS %%%%%%

colour('P', white).
colour('p', black).
colour('B', white).
colour('b', black).
colour('N', white).
colour('n', black).
colour('R', white).
colour('r', black).
colour('Q', white).
colour('q', black).
colour('K', white).
colour('k', black).
colour(' ', nocolour). 

reverse_colour(white, black).
reverse_colour(black, white).
reverse_colour(nocolour, nocolour).
%%%%% OUT %%%%%%

start_colours([[7,7,7,7,7,7,7,7], [7,7,7,7,7,7,7,7], [7,7,7,7,7,7,7,7], [7,7,7,7,7,7,7,7], [7,7,7,7,7,7,7,7], [7,7,7,7,7,7,7,7], [7,7,7,7,7,7,7,7], [7,7,7,7,7,7,7,7]]).


concat_colour([], _, []).

concat_colour([H|T], Colour, [Head|Tail]):-
	append([Colour], H, Head),
	concat_colour(T, Colour, Tail).

%%%%%%%%%%%%%%%%%%%%     FEN -> STATE      %%%%%%%%%%%%%%%%%%%%

initialize_state(InputFEN, ChessBoard):- %FIX
        fen_parser(InputFEN, InputList),
        concat_ranks(InputList,8,[],ChessBoard).  %8 iterations to take each rank form inp to state 

fen_parser(Input, OutState):-
        split_string(Input, "/", "", RanksList), %RankLists has 8 elems, 8th  elem contains otherargs(wb, en passant,...)
        nth0(7, RanksList, Rank8),
        split_string(Rank8," ", "", OtherArgs),  %Parse 8th elem. OtherArg has 6 elems
        list_butlast(RanksList, Ranks),
        append([Ranks,OtherArgs], OutState).

concat_ranks(_,0,C,C):-
        !.

concat_ranks([H|T],Iter,Prev,ChessBoard):-
        string_chars(H, Pieces),
        fix_spaces(Pieces,[], FixedPieces),
        append(Prev, [FixedPieces], NewPrev),
        NewIter is Iter-1,
        concat_ranks(T,NewIter,NewPrev,ChessBoard).

fix_spaces([],Same,Same):-
        !.

fix_spaces([H|T],Dummy,FixedPieces):-
        char_type(H, digit),
        atom_number(H,NumofSpaces),
        insert_spaces(NumofSpaces,Dummy,New),
        fix_spaces(T,New,FixedPieces).

fix_spaces([H|T],Dummy,FixedPieces):-
        char_type(H,alpha),
        append(Dummy,[H],New),
        fix_spaces(T,New,FixedPieces).

insert_spaces(0, Rest, Rest):-
        !.
insert_spaces(Iter,Dummy,Result):-
        append(Dummy,[' '],NewDummy),
        NewIter is Iter - 1,
        insert_spaces(NewIter,NewDummy, Result).

list_butlast([X|Xs], Ys) :-                 % use auxiliary predicate ...
   list_butlast_prev(Xs, Ys, X).            % ... which lags behind by one item

list_butlast_prev([], [], _). %SOURCE: https://stackoverflow.com/questions/16174681/how-to-delete-the-last-element-from-a-list-in-prolog
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-
   list_butlast_prev(Xs, Ys, X1).

