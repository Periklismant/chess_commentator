%%%%%%%%%%%%%%%%%%%     INCLUDES      %%%%%%%%%%%%%%%%%%%%%%%

:- ['./input_to_state.prolog'].
:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].
:- ['./game_phase_and_alive_pieces.prolog'].
:- ['./centre_control.prolog'].
:- ['./chessboard_pins.prolog'].

%%%%%%%%%%%%%%%%%%%      ChessBoard States     %%%%%%%%%%%%%%%%%%%%%

play_moves(InputFEN, Moves, [State | NextStates], Pins, Centre, Outfile):-
	initialize_state(InputFEN, State),
	open(Outfile, write, Stream),
	writeState(Stream, State),
	play_next_moves(State, Moves, NextStates, 0, Pins, Centre, Stream),
	close(Stream).

play_next_moves(_,[],[],_,[],[],_).

play_next_moves(State, [Move | NextMoves], [NewState | NextStates], MoveCount, [Pins | NextPins], [[Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScoreCentre, BlackScoreCentre] | NextCentre], Stream):-
	string_chars(Move, ParsedMove), 
        fix_indexes(ParsedMove, FixedMove),
        identify_piece(FixedMove, State, Piece,_,_),
        play_move(State, FixedMove, Piece,  NewState),
	NewMoveCount is MoveCount + 1,
	material_evaluation(NewState, 0, 0, WhiteScore, BlackScore),
	identify_game_phase(NewState, NewMoveCount, GamePhase),
	chessboard_pins(NewState, Pins),
	chessboard_centre_control(NewState, Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScoreCentre, BlackScoreCentre),
	nl(Stream),
	writeState(Stream, NewState),
	nl(Stream),
	write(Stream,"MATERIAL EVALUATION FOR WHITE: "),
	write(Stream, WhiteScore),
	nl(Stream),
	write(Stream,"MATERIAL EVALUATION FOR BLACK: "),
	write(Stream, BlackScore),
	nl(Stream),
	write(Stream,"GAME PHASE: "),
	write(Stream, GamePhase),
	nl(Stream),
	write(Stream,"PINS FOUND ON THE BOARD ABOVE ARE: "),
	write(Stream, Pins),
	nl(Stream),
	write(Stream, "CENTRE CONTROL: "),
	write(Stream, Side),
	nl(Stream),
	write(Stream, "WHITE FORCES CONTROLLING THE CENTRE: "),
	write(Stream, WhiteScoreCentre),
	nl(Stream),
	write(Stream, "BLACK FORCES CONTROLLING THE CENTRE: "),
	write(Stream, BlackScoreCentre),
	nl(Stream),
	play_next_moves(NewState, NextMoves, NextStates, NewMoveCount, NextPins, NextCentre, Stream).

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

