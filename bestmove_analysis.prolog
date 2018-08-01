%%%%%%%%%%%%%%%%%%%     INCLUDES      %%%%%%%%%%%%%%%%%%%%%%%

:- ['./input_to_state.prolog'].
:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].
:- ['./game_phase_and_alive_pieces.prolog'].
:- ['./centre_control.prolog'].
:- ['./pins_and_skewers.prolog'].
:- ['./attackers_and_defenders.prolog'].

%%%%%%%%%%%%%%%%%%%      ChessBoard States     %%%%%%%%%%%%%%%%%%%%%

play_moves(InputFEN, Moves, [State | NextStates], Pins, Centre, Outfile):-
	initialize_state(InputFEN, State, Next_to_Play, Castling, EnPassant, HalfMoves, FullMoves0),
	atom_number(FullMoves0, FullMoves),
	string_chars(Castling, CastlingList),
	state_move_identifier(Next_to_Play, FullMoves, MoveCount),
	player_to_colour(Next_to_Play, Next_Colour),
	open(Outfile, write, Stream),
	writeState(Stream, State),
	play_next_moves(State, Moves, NextStates, MoveCount, Next_Colour, CastlingList, Pins, Centre, Stream),
	close(Stream).

play_next_moves(_,[],[], _, _, _, [],[],_).

play_next_moves(State, [Move | NextMoves], [NewState | NextStates], MoveCount, Next_Colour, CastlingList, [Pins | NextPins], [[Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScoreCentre, BlackScoreCentre] | NextCentre], Stream):-
	string_chars(Move, ParsedMove), 
        fix_indexes(ParsedMove, FixedMove),
        identify_piece(FixedMove, State, Piece,_,_),
        play_move(State, FixedMove, Piece, NewState, CastlingList, NewCastlingList),
	NewMoveCount is MoveCount + 1,
	material_evaluation(NewState, 0, 0, WhiteScore, BlackScore),
	identify_game_phase(NewState, NewMoveCount, GamePhase),
	chessboard_pins_skewers(NewState, Pins, Skewers),
	chessboard_centre_control(NewState, Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScoreCentre, BlackScoreCentre),
	reverse_colour(Next_Colour, NewNext),
	identify_threats(NewState, NewNext, Threats),
	nl(Stream),
	writeState(Stream, NewState),
	nl(Stream),
	write(Stream,"PLAYER TO MOVE: "),
	write(Stream, NewNext),
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
	write(Stream, "THREATS FOR "),
	write(Stream, NewNext),
	write(Stream, " IN THE FORMAT [SQUARE, MATERIAL_THREATENED_TO_BE_LOST] ARE: "),
	write(Stream, Threats),
	nl(Stream),
	write(Stream,"PINS FOUND ON THE BOARD ABOVE ARE: "),
	write(Stream, Pins),
	nl(Stream),
	write(Stream,"SKEWERS FOUND ON THE BOARD ABOVE ARE: "),
	write(Stream, Skewers),
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
	play_next_moves(NewState, NextMoves, NextStates, NewMoveCount, NewNext, NewCastlingList, NextPins, NextCentre, Stream).

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

%%%%%%%%%%%%%%%%%%%%%%%     MOVE COUNT    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

state_move_identifier("w", FullMoves, MoveCount):-
	MoveCount is 2*FullMoves - 2.

state_move_identifier("b", FullMoves, MoveCount):-
	MoveCount is 2*FullMoves - 1.
	

%%%%%%%%%%%%%%%%%%%%%%%     PLAY MOVE     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_move(State, [0, 4, 0, 2], 'k', NewState, CastlingList, NewCastlingList):-
        queenside_white_available(CastlingList, available), !,
        delete(CastlingList, 'q', NewCastlingList),
        nth0(7, State, RanktoChange, BuffState),
        nth0(4, RanktoChange, 'k', BuffRank0),
        nth0(4, ChangedRank0, ' ', BuffRank0),
        nth0(0, ChangedRank0, 'r', BuffRank1),
        nth0(0, ChangedRank1, ' ', BuffRank1),
        nth0(2, ChangedRank1, ' ', BuffRank2),
        nth0(2, ChangedRank2, 'k', BuffRank2),
        nth0(3, ChangedRank2, ' ', BuffRank3),
        nth0(3, ChangedRank3, 'r', BuffRank3),
        nth0(7, NewState, ChangedRank3, BuffState).

play_move(State, [0, 4, 0, 6], 'k', NewState, CastlingList, NewCastlingList):-
	kingside_black_available(CastlingList, available), !,
	delete(CastlingList, 'k', NewCastlingList),
	nth0(0, State, RanktoChange, BuffState),
	nth0(4, RanktoChange, 'k', BuffRank0),
	nth0(4, ChangedRank0, ' ', BuffRank0),
	nth0(7, ChangedRank0, 'r', BuffRank1),
	nth0(7, ChangedRank1, ' ', BuffRank1),
	nth0(6, ChangedRank1, ' ', BuffRank2),
	nth0(6, ChangedRank2, 'k', BuffRank2),
	nth0(5, ChangedRank2, ' ', BuffRank3),
	nth0(5, ChangedRank3, 'r', BuffRank3),
	nth0(0, NewState, ChangedRank3, BuffState).

play_move(State, [7, 4, 7, 6], 'K', NewState, CastlingList, NewCastlingList):-
	kingside_white_available(CastlingList, available), !,
	delete(CastlingList, 'K', NewCastlingList),
	nth0(7, State, RanktoChange, BuffState),
	nth0(4, RanktoChange, 'K', BuffRank0),
	nth0(4, ChangedRank0, ' ', BuffRank0),
	nth0(7, ChangedRank0, 'R', BuffRank1),
	nth0(7, ChangedRank1, ' ', BuffRank1),
	nth0(6, ChangedRank1, ' ', BuffRank2),
	nth0(6, ChangedRank2, 'K', BuffRank2),
	nth0(5, ChangedRank2, ' ', BuffRank3),
	nth0(5, ChangedRank3, 'R', BuffRank3),
	nth0(7, NewState, ChangedRank3, BuffState).

play_move(State, [7, 4, 7, 2], 'K', NewState, CastlingList, NewCastlingList):-
	queenside_white_available(CastlingList, available), !,
	delete(CastlingList, 'Q', NewCastlingList),
	nth0(7, State, RanktoChange, BuffState),           
        nth0(4, RanktoChange, 'K', BuffRank0),
        nth0(4, ChangedRank0, ' ', BuffRank0),
        nth0(0, ChangedRank0, 'R', BuffRank1),
        nth0(0, ChangedRank1, ' ', BuffRank1),
        nth0(2, ChangedRank1, ' ', BuffRank2),
        nth0(2, ChangedRank2, 'K', BuffRank2),
        nth0(3, ChangedRank2, ' ', BuffRank3),
        nth0(3, ChangedRank3, 'R', BuffRank3),
	nth0(7, NewState, ChangedRank3, BuffState).

play_move(State,[OldRank, OldFile, NewRank, NewFile],Piece, NewState, CastlingList, CastlingList):-
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

