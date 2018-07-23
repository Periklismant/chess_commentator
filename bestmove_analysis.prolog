%%%%%%%%%%%%%%%%%%%     INCLUDES      %%%%%%%%%%%%%%%%%%%%%%%

%:- ['./RTEC-master/src/RTEC.prolog'].

%%%%%%%%%%%%%%%%%%%      ChessBoard States     %%%%%%%%%%%%%%%%%%%%%

play_moves(InputFEN, Moves, [State | NextStates], Pins, Centre, Outfile):-
	initialize_state(InputFEN, State),
	open(Outfile, write, Stream),
	writeState(Stream, State),
	play_next_moves(State, Moves, NextStates, Pins, Centre, Stream),
	close(Stream).

play_next_moves(_,[],[],[],[],_).

play_next_moves(State, [Move | NextMoves], [NewState | NextStates], [Pins | NextPins], [[Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScore, BlackScore] | NextCentre], Stream):-
	string_chars(Move, ParsedMove), 
        fix_indexes(ParsedMove, FixedMove),
        identify_piece(FixedMove, State, Piece,_,_),
        play_move(State, FixedMove, Piece,  NewState),
	chessboard_pins(NewState, Pins),
	chessboard_centre_control(NewState, Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScore, BlackScore),
	nl(Stream),
	writeState(Stream, NewState),
	nl(Stream),
	write(Stream,"PINS FOUND ON THE BOARD ABOVE ARE: "),
	write(Stream, Pins),
	nl(Stream),
	write(Stream, "CENTRE CONTROL: "),
	write(Stream, Side),
	nl(Stream),
	write(Stream, "WHITE FORCES CONTROLLING THE CENTRE: "),
	write(Stream, WhiteScore),
	nl(Stream),
	write(Stream, "BLACK FORCES CONTROLLING THE CENTRE: "),
	write(Stream, BlackScore),
	nl(Stream),
	play_next_moves(NewState, NextMoves, NextStates, NextPins, NextCentre, Stream).

%initially(ChessBoard = [['r'],['n']]).

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


%%%%%%%%%%%%%%%%%%%      Move_Analysis (Before-After for piece moved)     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

        append([ControlsTheCentre, Pins], Strategy).
	
%states_in_file(OutFile, State, NewState). %%  Colouring).	

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

%%%%%%%%%%%%%%%%%%%%%%     CENTRE CONTROL     %%%%%%%%%%%%%%%%%%%%%%%%%%%%
chessboard_centre_control(ChessBoard, Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScore, BlackScore):-
	identify_pieces_on_centre_squares(ChessBoard, Pieces_on_Centre_Squares),
	evaluate_centre_pieces(Pieces_on_Centre_Squares, 0, 0, WhiteScore1, BlackScore1),
	identify_pieces_controlling_centre(ChessBoard, 0, Pieces_Controlling_the_Centre, 0, 0, WhiteScore2, BlackScore2),
	WhiteScore is WhiteScore1 + WhiteScore2,
	BlackScore is BlackScore1 + BlackScore2, 
	check_centre_scores(WhiteScore, BlackScore, Side).

check_centre_scores(WhiteScore, BlackScore, white_controls_the_centre):-
	WhiteScore > BlackScore, !.

check_centre_scores(WhiteScore, BlackScore, black_controls_the_centre):-
	BlackScore > WhiteScore, !.

check_centre_scores(_, _, centre_equally_controlled).	

identify_pieces_controlling_centre(_, 8, [], WhiteScore, BlackScore, WhiteScore, BlackScore):- !.

identify_pieces_controlling_centre(ChessBoard, Rank, [Centre_Controlling_on_Rank | Tail], WTemp, BTemp, WhiteScore, BlackScore):-
	check_centre_control(ChessBoard, Rank, 0, Centre_Controlling_on_Rank, WTemp, BTemp, NewW, NewB), !,
	NewRank is Rank + 1, 
	identify_pieces_controlling_centre(ChessBoard, NewRank, Tail, NewW, NewB, WhiteScore, BlackScore).


check_centre_control(_, _, 8, [], WhiteScore, BlackScore, WhiteScore, BlackScore):- !.

check_centre_control(ChessBoard, Rank, File, [[Piece,Square]|T] , WTemp, BTemp, WhiteScore, BlackScore):-
	identify_piece([Rank,File], ChessBoard, Piece),
	piece_real_control_squares(Piece, Rank, File,ChessBoard, CtrlSquares),  
	controls_a_centre_square(CtrlSquares),!,
	index_to_square([Rank,File], Square),
	centre_control_squares(Piece, CtrlSquares, WTemp, BTemp, NewW, NewB),
	NewFile is File + 1,
	check_centre_control(ChessBoard, Rank, NewFile, T, NewW, NewB, WhiteScore, BlackScore).

	
check_centre_control(ChessBoard, Rank, File, Pieces_Controlling_the_Centre, WTemp, BTemp, WhiteScore, BlackScore):-
	NewFile is File + 1,
	check_centre_control(ChessBoard, Rank, NewFile, Pieces_Controlling_the_Centre, WTemp, BTemp, WhiteScore, BlackScore).

controls_a_centre_square([H|_]):-
	centre_square(H),!.

controls_a_centre_square([_|T]):-
	controls_a_centre_square(T).

centre_control_squares(_,[], WhiteScore, BlackScore, WhiteScore, BlackScore).

centre_control_squares(Piece, [H|T], WTemp, BTemp, WhiteScore, BlackScore):-
	centre_square(H),
	colour(Piece, white),!,
	NewW is WTemp + 1, 
	centre_control_squares(Piece, T, NewW, BTemp, WhiteScore, BlackScore).	

centre_control_squares(Piece, [H|T], WTemp, BTemp, WhiteScore, BlackScore):-
	centre_square(H),!,
	NewB is BTemp + 1,
	centre_control_squares(Piece, T, WTemp, NewB, WhiteScore, BlackScore).	

centre_control_squares(Piece, [_|T], WTemp, BTemp, WhiteScore, BlackScore):-
	centre_control_squares(Piece, T, WTemp, BTemp, WhiteScore, BlackScore).

%% CHECK AGAIN %% 

evaluate_centre_pieces([], WhiteScore, BlackScore, WhiteScore, BlackScore).

evaluate_centre_pieces([[Piece,_]|T], WTemp, BTemp,  WhiteScore, BlackScore):-
	colour(Piece, white),!,
	NewW is WTemp + 1,
	evaluate_centre_pieces(T, NewW, BTemp, WhiteScore, BlackScore). 

evaluate_centre_pieces([_ | T], WTemp, BTemp, WhiteScore, BlackScore):-
	NewB is BTemp + 1,
	evaluate_centre_pieces(T, WTemp, NewB, WhiteScore, BlackScore).	

identify_pieces_on_centre_squares(ChessBoard, CentrePieces):-
	identify_piece([3,3], ChessBoard, D5Piece),
	identify_piece([3,4], ChessBoard, E5Piece),
	identify_piece([4,3], ChessBoard, D4Piece),
	identify_piece([4,4], ChessBoard, E4Piece),
	CentreTemp = [[D5Piece,"d5"], [E5Piece, "e5"],[D4Piece, "d4"],[E4Piece, "e4"]],
	fix_pieces_on_centre_squares(CentreTemp, CentrePieces).

fix_pieces_on_centre_squares([],[]).

fix_pieces_on_centre_squares([[Piece, _]|T], CentrePieces):-
	Piece  = ' ',
	!,
	fix_pieces_on_centre_squares(T, CentrePieces).

fix_pieces_on_centre_squares([H|T], [H|Tail]):-
	fix_pieces_on_centre_squares(T, Tail).

identify_centre_control(Piece, Rank, File, NewRank, NewFile, State, NewState, ControlsTheCentre):-	
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

centre_square([3,3]).
centre_square([3,4]).
centre_square([4,3]).
centre_square([4,4]).

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

%%%%%%%%%%%%%%%%%%%%%%%     GAME PHASES     %%%%%%%%%%%%%%%%%%%%%%%%%%
identify_game_phase(_, MovesNo, opening):-
	MovesNo < 20, !.

identify_game_phase(ChessBoard, _, middlegame):-
        traded_queens(ChessBoard, notTraded), 
	find_material_with_evaluation(ChessBoard, [_, [WhiteKnights, WhiteLight, WhiteDark],[WhiteRooks],[_]],[_, [BlackKnights, BlackLight, BlackDark], [BlackRooks], [_]], _, _),
	S is WhiteKnights + WhiteLight + WhiteDark + WhiteRooks + BlackKnights + BlackLight + BlackDark + BlackRooks,
	S > 2, !.

identify_game_phase(ChessBoard, _, endgame):-
	traded_queens(ChessBoard, notTraded), !.

identify_game_phase(ChessBoard, _, queenless_middlegame):-
	traded_queens(ChessBoard, traded),
	find_material_with_evaluation(ChessBoard, [_, [WhiteKnights, WhiteLight, WhiteDark],[WhiteRooks],[_]],[_, [BlackKnights, BlackLight, BlackDark], [BlackRooks], [_]], _, _),
	S is WhiteKnights + WhiteLight + WhiteDark + WhiteRooks + BlackKnights + BlackLight + BlackDark + BlackRooks,
	S > 8, !.	


identify_game_phase(ChessBoard, _, endgame):-
        traded_queens(ChessBoard, traded).
	
traded_queens([], traded).

traded_queens([H|T], Queens):-
	no_queens_on_rank(H),!,
	traded_queens(T, Queens).

traded_queens([_|_], notTraded).

no_queens_on_rank([]).

no_queens_on_rank(['Q'|_]):-
	!,fail.
no_queens_on_rank(['q'|_]):-
	!, fail.

no_queens_on_rank([_|T]):-
	no_queens_on_rank(T).

%%%%%%%%%%%%%%%%%%%%%%      FIND ALIVE PIECES     %%%%%%%%%%%%%%%%%%%%%%%%%%%
find_material_with_evaluation(ChessBoard, [[WAPawn, WBPawn, WCPawn, WDPawn, WEPawn, WFPawn, WGPawn, WHPawn], [WhiteKnights, WhiteLight, WhiteDark], [WhiteRooks], [WhiteQueens]],[[BAPawn, BBPawn, BCPawn, BDPawn, BEPawn, BFPawn, BGPawn, BHPawn], [BlackKnights, BlackLight, BlackDark], [BlackRooks], [BlackQueens]], WhiteScore, BlackScore):-
	find_alive_pawns(ChessBoard, WAPawn, WBPawn, WCPawn, WDPawn, WEPawn, WFPawn, WGPawn, WHPawn, BAPawn, BBPawn, BCPawn, BDPawn,BEPawn, BFPawn, BGPawn, BHPawn),
	find_alive_pieces(ChessBoard, WhiteKnights, BlackKnights, WhiteLight, WhiteDark, BlackLight, BlackDark, WhiteRooks, BlackRooks, WhiteQueens, BlackQueens),
	material_evaluation(ChessBoard, 0, 0, WhiteScore, BlackScore).	

find_alive_pawns(ChessBoard, WAPawn, WBPawn, WCPawn, WDPawn, WEPawn, WFPawn, WGPawn, WHPawn, BAPawn, BBPawn, BCPawn, BDPawn,BEPawn, BFPawn, BGPawn, BHPawn):-
	find_nth_pawn(ChessBoard, 0, 0, 0, WAPawn, BAPawn),
	find_nth_pawn(ChessBoard, 1, 0, 0, WBPawn, BBPawn),
	find_nth_pawn(ChessBoard, 2, 0, 0, WCPawn, BCPawn),
	find_nth_pawn(ChessBoard, 3, 0, 0, WDPawn, BDPawn),
	find_nth_pawn(ChessBoard, 4, 0, 0, WEPawn, BEPawn),
	find_nth_pawn(ChessBoard, 5, 0, 0, WFPawn, BFPawn),
	find_nth_pawn(ChessBoard, 6, 0, 0, WGPawn, BGPawn),
	find_nth_pawn(ChessBoard, 7, 0, 0, WHPawn, BHPawn).

find_nth_pawn([], _, CountW, CountB, CountW, CountB).

find_nth_pawn([H|T], N, WTemp, BTemp, CountW, CountB):-
	find_nth_pawn_rank(H, N, WTemp, BTemp, NewW, NewB),
	find_nth_pawn(T, N, NewW, NewB, CountW, CountB).
	
find_nth_pawn_rank(RankList, N, WTemp, BTemp, WTemp, NewB):-
	nth0(N, RankList, 'p'), !,
	NewB is BTemp + 1.

find_nth_pawn_rank(RankList, N, WTemp, BTemp, NewW, BTemp):-
        nth0(N, RankList, 'P'), !,
        NewW is WTemp + 1.
	
find_nth_pawn_rank(_, _, W, B, W, B).
	
find_alive_pieces(ChessBoard, WhiteKnights, BlackKnights, WhiteLight, WhiteDark, BlackLight, BlackDark, WhiteRooks, BlackRooks, WhiteQueens, BlackQueens):-
	find_alive_knights(ChessBoard, 0, 0, WhiteKnights, BlackKnights),
	find_alive_rooks(ChessBoard, 0, 0, WhiteRooks, BlackRooks),
	find_alive_queens(ChessBoard, 0, 0, WhiteQueens, BlackQueens),
	find_alive_bishops(ChessBoard, 0, 0, 0, 0, 0, 0, WhiteLight, WhiteDark, BlackLight, BlackDark).

find_alive_bishops(_,8,_, WL, WD, BL, BD, WL, WD, BL, BD):- !.

find_alive_bishops(ChessBoard, Rank, File, WLTemp, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark):-
	nth0(Rank, ChessBoard, RankList),
	find_bishops_rank(RankList, Rank, File, WLTemp, WDTemp, BLTemp, BDTemp, NewWL, NewWD, NewBL, NewBD),
	NewRank is Rank + 1,
	find_alive_bishops(ChessBoard, NewRank, File, NewWL, NewWD, NewBL, NewBD, WhiteLight, WhiteDark, BlackLight, BlackDark).

find_bishops_rank(_,_,8, WL, WD, BL, BD, WL, WD, BL, BD):- !.

find_bishops_rank(RankList, Rank, File, WLTemp, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark):-
	nth0(File, RankList, 'B'),
	colour(Rank, File, light), !, 
	NewWL is WLTemp + 1, 
	NewFile is File + 1,
	find_bishops_rank(RankList, Rank, NewFile, NewWL, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark).

find_bishops_rank(RankList, Rank, File, WLTemp, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark):-
        nth0(File, RankList, 'B'),
        colour(Rank, File, dark), !,
        NewWD is WDTemp + 1,
        NewFile is File + 1,
        find_bishops_rank(RankList, Rank, NewFile, WLTemp, NewWD, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark).

find_bishops_rank(RankList, Rank, File, WLTemp, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark):-
        nth0(File, RankList, 'b'),
        colour(Rank, File, light), !,
        NewBL is BLTemp + 1,
        NewFile is File + 1,
        find_bishops_rank(RankList, Rank, NewFile, WLTemp, WDTemp, NewBL, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark).

find_bishops_rank(RankList, Rank, File, WLTemp, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark):-
        nth0(File, RankList, 'b'),
        colour(Rank, File, dark), !,
        NewBD is BDTemp + 1,
        NewFile is File + 1,
        find_bishops_rank(RankList, Rank, NewFile, WLTemp, WDTemp, BLTemp, NewBD, WhiteLight, WhiteDark, BlackLight, BlackDark).

find_bishops_rank(RankList, Rank, File, WLTemp, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark):-
	NewFile is File + 1,
	find_bishops_rank(RankList, Rank, NewFile, WLTemp, WDTemp, BLTemp, BDTemp, WhiteLight, WhiteDark, BlackLight, BlackDark).

find_alive_queens([], WhiteQueens, BlackQueens, WhiteQueens, BlackQueens).

find_alive_queens([H|T], WTemp, BTemp, WhiteQueens, BlackQueens):-
	find_queens_rank(H, WTemp, BTemp, NewW, NewB),
	find_alive_queens(T, NewW, NewB, WhiteQueens, BlackQueens).

find_queens_rank([], WhiteQueens, BlackQueens, WhiteQueens, BlackQueens).

find_queens_rank(['q'|T], WTemp, BTemp, WhiteQueens, BlackQueens):-
	NewB is BTemp + 1, !, 
	find_queens_rank(T, WTemp, NewB, WhiteQueens, BlackQueens).
	
find_queens_rank(['Q'|T], WTemp, BTemp, WhiteQueens, BlackQueens):-
	NewW is WTemp + 1, !, 
	find_queens_rank(T, NewW, BTemp, WhiteQueens, BlackQueens).

find_queens_rank([_|T], WTemp, BTemp, WhiteQueens, BlackQueens):- 
	find_queens_rank(T, WTemp, BTemp, WhiteQueens, BlackQueens).

find_alive_knights([], WhiteKnights, BlackKnights, WhiteKnights, BlackKnights).

find_alive_knights([H|T], WTemp, BTemp, WhiteKnights, BlackKnights):-
	find_knights_rank(H, WTemp, BTemp, NewW, NewB),
	find_alive_knights(T, NewW, NewB, WhiteKnights, BlackKnights).  
	
find_knights_rank([], WhiteKnights, BlackKnights, WhiteKnights, BlackKnights).

find_knights_rank(['n'|T], WTemp, BTemp, WhiteKnights, BlackKnights):-
	NewB is BTemp + 1, !, 
	find_knights_rank(T, WTemp, NewB, WhiteKnights, BlackKnights).
	
find_knights_rank(['N'|T], WTemp, BTemp, WhiteKnights, BlackKnights):-
	NewW is WTemp + 1, !,
	find_knights_rank(T, NewW, BTemp, WhiteKnights, BlackKnights).

find_knights_rank([_|T], WTemp, BTemp, WhiteKnights, BlackKnights):-
	find_knights_rank(T, WTemp, BTemp, WhiteKnights, BlackKnights).

find_alive_rooks([], WhiteMinorPieces, BlackMinorPieces, WhiteMinorPieces, BlackMinorPieces).

find_alive_rooks([H|T], WTemp, BTemp, WhiteMinorPieces, BlackMinorPieces):-
	find_rooks_rank(H, WTemp, BTemp, NewW, NewB),
	find_alive_rooks(T, NewW, NewB, WhiteMinorPieces, BlackMinorPieces).

find_rooks_rank([], WhiteMinorPieces, BlackMinorPieces, WhiteMinorPieces, BlackMinorPieces).

find_rooks_rank(['r'|T], WTemp, BTemp, WhiteMinorPieces, BlackMinorPieces):-
	NewB is BTemp + 1, !,
	find_rooks_rank(T, WTemp, NewB, WhiteMinorPieces, BlackMinorPieces).
	
find_rooks_rank(['R'|T], WTemp, BTemp, WhiteMinorPieces, BlackMinorPieces):-
	NewW is WTemp + 1, !,
	find_rooks_rank(T, NewW, BTemp, WhiteMinorPieces, BlackMinorPieces).

find_rooks_rank([_|T], WTemp, BTemp, WhiteMinorPieces, BlackMinorPieces):-
	find_rooks_rank(T, WTemp, BTemp, WhiteMinorPieces, BlackMinorPieces).


%%%%%%%%%%%%%%%%%%%%%%%     MATERIAL EVAL      %%%%%%%%%%%%%%%%%%%%%%%%%%

material_evaluation([], WhiteScore, BlackScore, WhiteScore, BlackScore).

material_evaluation([H|T], WHelp, BHelp, WhiteScore, BlackScore):-
	material_on_rank(H, WHelp, BHelp, NewW, NewB), 
	material_evaluation(T, NewW, NewB, WhiteScore, BlackScore).

material_on_rank([], WhiteScore, BlackScore, WhiteScore, BlackScore).

material_on_rank(['K'|T], WHelp, BHelp, WhiteScore, BlackScore):-
	!,
	material_on_rank(T, WHelp, BHelp, WhiteScore, BlackScore).

material_on_rank(['k'|T], WHelp, BHelp, WhiteScore, BlackScore):-
	!,
	material_on_rank(T, WHelp, BHelp, WhiteScore, BlackScore).

material_on_rank([' '|T], WHelp, BHelp, WhiteScore, BlackScore):-
	!,
	material_on_rank(T, WHelp, BHelp, WhiteScore, BlackScore).

material_on_rank([H|T], WHelp, BHelp, WhiteScore, BlackScore):-
	colour(H, white), !,
	evaluate(H, Eval),
	NewW is WHelp + Eval,
	material_on_rank(T, NewW, BHelp, WhiteScore, BlackScore).

material_on_rank([H|T], WHelp, BHelp, WhiteScore, BlackScore):-
	evaluate(H, Eval),
	NewB is BHelp + Eval, 
	material_on_rank(T, WHelp, NewB, WhiteScore, BlackScore).

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

piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquares):-
 	piece_control_squares(Piece, Rank, File, AllPossibleCtrlSquares),
        remove_blocked_squares(Piece, Rank, File, ChessBoard, AllPossibleCtrlSquares, BlockedList, []),
        subtract(AllPossibleCtrlSquares, BlockedList, CtrlSquares).


remove_blocked_squares('R',_ , _, _, [], BlockedRes,BlockedRes):- !. 

remove_blocked_squares('R', Rank, File, State, [H|T], BlockedRes, BlockedList):-
	member(H, BlockedList),!,
	remove_blocked_squares('R',Rank, File, State, T, BlockedRes, BlockedList).

remove_blocked_squares('R', Rank, File, State, [H|T], BlockedRes, BlockedList):-
	identify_piece(H, State,' '),!,
	remove_blocked_squares('R',Rank, File, State,T, BlockedRes, BlockedList).

remove_blocked_squares('R', Rank, File, State, [H|T], BlockedRes, BlockedList):-
	!,find_relative_positionR(Rank, File, H, Direction),
	fix_blocked_list(H,Direction,NewBlocked),
	append(BlockedList, NewBlocked, NewBlockedList),
	remove_blocked_squares('R', Rank, File, State, T,BlockedRes, NewBlockedList).

remove_blocked_squares('r',_ , _, _, [], BlockedRes,BlockedRes):- !.

remove_blocked_squares('r', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        member(H, BlockedList),!,
        remove_blocked_squares('r',Rank, File, State, T, BlockedRes, BlockedList).

remove_blocked_squares('r', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        identify_piece(H, State,' '),!,
        remove_blocked_squares('r',Rank, File, State,T, BlockedRes, BlockedList).

remove_blocked_squares('r', Rank, File, State, [H|T], BlockedRes, BlockedList):-
       !, find_relative_positionR(Rank, File, H, Direction),
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
        member(H, BlockedList), !,
        remove_blocked_squares('B',Rank, File, State, T, BlockedRes, BlockedList).

remove_blocked_squares('B', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        identify_piece(H, State,' '),!,
        remove_blocked_squares('B',Rank, File, State,T, BlockedRes, BlockedList).

remove_blocked_squares('B', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        !,find_relative_positionB(Rank, File, H, Direction),
        fix_blocked_list(H,Direction,NewBlocked),
        append(BlockedList, NewBlocked, NewBlockedList),
        remove_blocked_squares('B', Rank, File, State, T,BlockedRes, NewBlockedList).

remove_blocked_squares('b',_ , _, _, [], BlockedRes,BlockedRes):- !.

remove_blocked_squares('b', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        member(H, BlockedList), !,
        remove_blocked_squares('b',Rank, File, State, T, BlockedRes, BlockedList).

remove_blocked_squares('b', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        identify_piece(H, State,' '),!,
        remove_blocked_squares('b',Rank, File, State,T, BlockedRes, BlockedList).

remove_blocked_squares('b', Rank, File, State, [H|T], BlockedRes, BlockedList):-
        !,find_relative_positionB(Rank, File, H, Direction),
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
	!,
	seperate_possible(AllPossible, Rank, File, AllDiagonal, AllHorVert),
	remove_blocked_squares('B', Rank, File, State, AllDiagonal, BlockedRes1, BlockedList),
	remove_blocked_squares('R', Rank, File, State, AllHorVert, BlockedRes2, BlockedList),
	append(BlockedRes1, BlockedRes2, BlockedRes).

remove_blocked_squares('q',Rank, File, State, AllPossible, BlockedRes, BlockedList):-
	!,
        seperate_possible(AllPossible, Rank, File, AllDiagonal, AllHorVert),
        remove_blocked_squares('b', Rank, File, State, AllDiagonal, BlockedRes1, BlockedList),
        remove_blocked_squares('r', Rank, File, State, AllHorVert, BlockedRes2, BlockedList),
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
	piece_control_squares('b', Rank, File, CtrlSquares1),
	piece_control_squares('r',Rank, File, CtrlSquares2 ),
	append(CtrlSquares1, CtrlSquares2, CtrlSquares). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     KNIGHTS     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

piece_control_squares('n',Rank, File, CtrlSquares):-
        oneOClock_Square(Rank,File,OneResult),
        twoOClock_Square(Rank,File,TwoResult),
        fourOClock_Square(Rank,File,FourResult),
        fiveOClock_Square(Rank,File,FiveResult),
        sevenOClock_Square(Rank,File,SevenResult),
        eightOClock_Square(Rank,File,EightResult),
        tenOClock_Square(Rank,File,TenResult),
        elevenOClock_Square(Rank,File,ElevenResult),
        append([OneResult,TwoResult,FourResult,FiveResult,SevenResult,EightResult,TenResult,ElevenResult], CtrlSquares).

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

%%%%%%%%%%%%%%%%%%%%     COORDINATES      %%%%%%%%%%%%%%%%%%%

index_to_square([0,0], "a8").
index_to_square([0,1],"b8").
index_to_square([0,2],"c8").
index_to_square([0,3],"d8").
index_to_square([0,4],"e8").
index_to_square([0,5],"f8").
index_to_square([0,6],"g8").
index_to_square([0,7],"h8").
index_to_square([1,0],"a7").
index_to_square([1,1],"b7").
index_to_square([1,2],"c7").
index_to_square([1,3],"d7").
index_to_square([1,4],"e7").
index_to_square([1,5],"f7").
index_to_square([1,6],"g7").
index_to_square([1,7],"h7").
index_to_square([2,0],"a6").
index_to_square([2,1],"b6").
index_to_square([2,2],"c6").
index_to_square([2,3],"d6").
index_to_square([2,4],"e6").
index_to_square([2,5],"f6").
index_to_square([2,6],"g6").
index_to_square([2,7],"h6").
index_to_square([3,0],"a5").
index_to_square([3,1],"b5").
index_to_square([3,2],"c5").
index_to_square([3,3],"d5").
index_to_square([3,4],"e5").
index_to_square([3,5],"f5").
index_to_square([3,6],"g5").
index_to_square([3,7],"h5").
index_to_square([4,0],"a4").
index_to_square([4,1],"b4").
index_to_square([4,2],"c4").
index_to_square([4,3],"d4").
index_to_square([4,4],"e4").
index_to_square([4,5],"f4").
index_to_square([4,6],"g4").
index_to_square([4,7],"h4").
index_to_square([5,0],"a3").
index_to_square([5,1],"b3").
index_to_square([5,2],"c3").
index_to_square([5,3],"d3").
index_to_square([5,4],"e3").
index_to_square([5,5],"f3").
index_to_square([5,6],"g3").
index_to_square([5,7],"h3").
index_to_square([6,0],"a2").
index_to_square([6,1],"b2").
index_to_square([6,2],"c2").
index_to_square([6,3],"d2").
index_to_square([6,4],"e2").
index_to_square([6,5],"f2").
index_to_square([6,6],"g2").
index_to_square([6,7],"h2").
index_to_square([7,0],"a1").
index_to_square([7,1],"b1").
index_to_square([7,2],"c1").
index_to_square([7,3],"d1").
index_to_square([7,4],"e1").
index_to_square([7,5],"f1").
index_to_square([7,6],"g1").
index_to_square([7,7],"h1").

%%%%%%%%%%%%%%%%%%%%      EVALS     %%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate('R', 5).
evaluate('r', 5).
evaluate('B', 3).
evaluate('b', 3).
evaluate('N', 3).
evaluate('n', 3).
evaluate('K', whiteking).
evaluate('k', blackking).
evaluate('Q', 9).
evaluate('q', 9).
evaluate('P', 1).
evaluate('p', 1).
evaluate(' ', 0).

%%%%%%%%%%%%%%%%%%%%     COLOUR     %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% CHESS %%%%%%
%%%%% PIECES %%%%%
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
%%%%% SQUARES %%%%%
colour(Rank, File, light):-
	S is Rank + File,
	K is S mod 2,
	K is 0, !.

colour(_, _, dark).



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

