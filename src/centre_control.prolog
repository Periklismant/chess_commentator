%%%%%%%%%%%%%%%  INCLUDES  %%%%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].


%%%%%%%%%%%%%%%%%%%%%%     CENTRE CONTROL     %%%%%%%%%%%%%%%%%%%%%%%%%%%%
chessboard_centre_control(ChessBoard, Side, Pieces_on_Centre_Squares, Pieces_Controlling_the_Centre, WhiteScore, BlackScore):-
        identify_pieces_on_centre_squares(ChessBoard, Pieces_on_Centre_Squares),
        evaluate_centre_pieces(Pieces_on_Centre_Squares, 0, 0, WhiteScore1, BlackScore1),
        identify_pieces_controlling_centre(ChessBoard, 0, Pieces_Controlling_the_Centre0, 0, 0, WhiteScore2, BlackScore2),
	append(Pieces_Controlling_the_Centre0, Pieces_Controlling_the_Centre),
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

check_centre_control(ChessBoard, Rank, File, [[Piece, Square]|T], WTemp, BTemp, WhiteScore, BlackScore):-
        identify_piece([Rank, File], ChessBoard, Piece),
        queen(Piece),
        colour(Piece, Colour),
        piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquaresImmediate),
	seperate_possible(CtrlSquaresImmediate, Rank, File, CtrlSquaresDiag, CtrlSquaresHorVert), 
        identify_diagonal_batteries(Colour, Rank, File, ChessBoard, CtrlSquaresDiag, CtrlSquaresFinalDiag),
	identify_line_batteries(Colour, Rank, File, ChessBoard, CtrlSquaresHorVert, CtrlSquaresFinalHorVert),
	append(CtrlSquaresFinalDiag, CtrlSquaresFinalHorVert, CtrlSquaresFinal), 
        controls_a_centre_square(CtrlSquaresFinal), !,
        index_to_square([Rank, File], Square),
        centre_control_squares(Piece, CtrlSquaresFinal, WTemp, BTemp, NewW, NewB),
        NewFile is File + 1,
        check_centre_control(ChessBoard, Rank, NewFile, T, NewW, NewB, WhiteScore, BlackScore).

check_centre_control(ChessBoard, Rank, File, [[Piece, Square]|T], WTemp, BTemp, WhiteScore, BlackScore):-
        identify_piece([Rank, File], ChessBoard, Piece),
        diagonal_piece(Piece), 
        colour(Piece, Colour),
        piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquaresImmediate),
        identify_diagonal_batteries(Colour, Rank, File, ChessBoard, CtrlSquaresImmediate, CtrlSquaresFinal),
        controls_a_centre_square(CtrlSquaresFinal), !,
        index_to_square([Rank, File], Square),
        centre_control_squares(Piece, CtrlSquaresFinal, WTemp, BTemp, NewW, NewB),
        NewFile is File + 1,
        check_centre_control(ChessBoard, Rank, NewFile, T, NewW, NewB, WhiteScore, BlackScore).

check_centre_control(ChessBoard, Rank, File, [[Piece, Square]|T], WTemp, BTemp, WhiteScore, BlackScore):-
	identify_piece([Rank, File], ChessBoard, Piece),
	line_piece(Piece),
	colour(Piece, Colour), 
	piece_real_control_squares(Piece, Rank, File, ChessBoard, CtrlSquaresImmediate),
	identify_line_batteries(Colour, Rank, File, ChessBoard, CtrlSquaresImmediate, CtrlSquaresFinal), 
	controls_a_centre_square(CtrlSquaresFinal), !,
	index_to_square([Rank, File], Square),
	centre_control_squares(Piece, CtrlSquaresFinal, WTemp, BTemp, NewW, NewB), 
	NewFile is File + 1,
	check_centre_control(ChessBoard, Rank, NewFile, T, NewW, NewB, WhiteScore, BlackScore).

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


%%%%%%% FOR BATTERIES %%%%%%

identify_line_batteries(Colour, Rank, File, ChessBoard, CtrlSquares, CtrlSquaresFinal):-
	check_for_line_batteries(Colour, Rank, File, ChessBoard, CtrlSquares, CtrlSquaresFinal0),
	append(CtrlSquaresFinal0, CtrlSquaresFinal).

check_for_line_batteries(_, _, _, _, [], []):- !.

check_for_line_batteries(Colour, Rank, File, ChessBoard, [[CtrlRank, CtrlFile]|T], [CurrentBatteryCtrl | Tail]):-
	identify_piece([CtrlRank, CtrlFile], ChessBoard, CtrlPiece),
	line_piece(CtrlPiece),
	colour(CtrlPiece, Colour), !,
	find_relative_positionR(Rank, File, [CtrlRank, CtrlFile], Direction),
	find_line_squares_direction(Direction, Colour, CtrlRank, CtrlFile, ChessBoard, MyCtrlSquares),
	append(MyCtrlSquares, [[CtrlRank, CtrlFile]], CurrentBatteryCtrl),
	check_for_line_batteries(Colour, Rank, File, ChessBoard, T, Tail).

check_for_line_batteries(Colour, Rank, File, ChessBoard, [H|T], [[H]|Tail]):-
	check_for_line_batteries(Colour, Rank, File, ChessBoard, T, Tail).

find_line_squares_direction(north, _, 0, _, _, []):- !.

find_line_squares_direction(north, Colour, Rank, File, ChessBoard, [[NewRank, File]|Tail]):-
	NewRank is Rank - 1,
	identify_piece([NewRank, File], ChessBoard, ' '), !,
	find_line_squares_direction(north, Colour, NewRank, File, ChessBoard, Tail).

find_line_squares_direction(north, Colour, Rank, File, ChessBoard, [[NewRank, File] |Tail]):-
	NewRank is Rank - 1,
	identify_piece([NewRank, File], ChessBoard, LinePiece),
	line_piece(LinePiece), 
	colour(LinePiece, Colour), !,
	find_line_squares_direction(north, Colour, NewRank, File, ChessBoard, Tail).

find_line_squares_direction(north, Colour, Rank, File, ChessBoard, [[NewRank, File]|Tail]):-
	NewRank is Rank - 1,
	find_line_squares_direction(north, Colour, 0, File, ChessBoard, Tail).

find_line_squares_direction(south, _, 7, _, _, []):- !.

find_line_squares_direction(south, Colour, Rank, File, ChessBoard, [[NewRank, File]|Tail]):-
	NewRank is Rank + 1,
	identify_piece([NewRank, File], ChessBoard, ' '), !,
	find_line_squares_direction(south, Colour, NewRank, File, ChessBoard, Tail).

find_line_squares_direction(south, Colour, Rank, File, ChessBoard, [[NewRank, File]|Tail]):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], ChessBoard, LinePiece), 
	line_piece(LinePiece),
	colour(LinePiece, Colour), !,
        find_line_squares_direction(south, Colour, NewRank, File, ChessBoard, Tail).

find_line_squares_direction(south, Colour, Rank, File, ChessBoard, [[NewRank, File]|Tail]):-
	NewRank is Rank + 1,
	find_line_squares_direction(south, Colour, 7, File, ChessBoard, Tail).

find_line_squares_direction(west, _, _, 0, _, []):- !.

find_line_squares_direction(west, Colour, Rank, File, ChessBoard, [[Rank, NewFile]|Tail]):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        find_line_squares_direction(west, Colour, Rank, NewFile, ChessBoard, Tail).

find_line_squares_direction(west, Colour, Rank, File, ChessBoard, [[Rank, NewFile]|Tail]):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, LinePiece), 
	line_piece(LinePiece), 
	colour(LinePiece, Colour), !,
        find_line_squares_direction(west, Colour, Rank, NewFile, ChessBoard, Tail).

find_line_squares_direction(west, Colour, Rank, File, ChessBoard, [[Rank, NewFile]|Tail]):-
        NewFile is File - 1,
        find_line_squares_direction(west, Colour, Rank, 0, ChessBoard, Tail).

find_line_squares_direction(east, _, _, 7, _, []):- !.

find_line_squares_direction(east, Colour, Rank, File, ChessBoard, [[Rank, NewFile]|Tail]):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        find_line_squares_direction(east, Colour, Rank, NewFile, ChessBoard, Tail).

find_line_squares_direction(east, Colour, Rank, File, ChessBoard, [[Rank, NewFile]|Tail]):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, LinePiece), 
	line_piece(LinePiece), 
	colour(LinePiece, Colour), !,
        find_line_squares_direction(east, Colour, Rank, NewFile, ChessBoard, Tail).

find_line_squares_direction(east, Colour, Rank, File, ChessBoard, [[Rank, NewFile]|Tail]):-
        NewFile is File + 1,
        find_line_squares_direction(east, Colour, Rank, 7, ChessBoard, Tail).

identify_diagonal_batteries(Colour, Rank, File, ChessBoard, CtrlSquares, CtrlSquaresFinal):-
        check_for_diagonal_batteries(Colour, Rank, File, ChessBoard, CtrlSquares, CtrlSquaresFinal0),
        append(CtrlSquaresFinal0, CtrlSquaresFinal).

check_for_diagonal_batteries(_, _, _, _, [], []):- !.

check_for_diagonal_batteries(Colour, Rank, File, ChessBoard, [[CtrlRank, CtrlFile]|T], [CurrentBatteryCtrl | Tail]):-
        identify_piece([CtrlRank, CtrlFile], ChessBoard, CtrlPiece),
        diagonal_piece(CtrlPiece),
        colour(CtrlPiece, Colour), !,
        find_relative_positionB(Rank, File, [CtrlRank, CtrlFile], Direction),
        find_diagonal_squares_direction(Direction, Colour, CtrlRank, CtrlFile, ChessBoard, MyCtrlSquares),
        append(MyCtrlSquares, [[CtrlRank, CtrlFile]], CurrentBatteryCtrl),
        check_for_diagonal_batteries(Colour, Rank, File, ChessBoard, T, Tail).

check_for_diagonal_batteries(Colour, Rank, File, ChessBoard, [H|T], [[H]|Tail]):-
	check_for_diagonal_batteries(Colour, Rank, File, ChessBoard, T, Tail).

find_diagonal_squares_direction(upleft, _, 0, _, _, []):- !.
find_diagonal_squares_direction(upleft, _, _, 0, _, []):- !.

find_diagonal_squares_direction(upleft, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
	NewRank is Rank - 1, 
	NewFile is File - 1,
	identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
	find_diagonal_squares_direction(upleft, Colour, NewRank, NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(upleft, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
	NewRank is Rank - 1,
	NewFile is File - 1, 
	identify_piece([NewRank, NewFile], ChessBoard, DiagPiece),
	diagonal_piece(DiagPiece), 
	colour(DiagPiece, Colour), !,
	find_diagonal_squares_direction(upleft, Colour, NewRank , NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(upleft, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
	NewRank is Rank - 1,
	NewFile is File - 1,
	find_diagonal_squares_direction(upleft, Colour, 0, 0, ChessBoard, Tail).

find_diagonal_squares_direction(upright, _, 0, _, _, []):- !.
find_diagonal_squares_direction(upright, _, _, 7, _, []):- !.

find_diagonal_squares_direction(upright, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_diagonal_squares_direction(upright, Colour, NewRank, NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(upright, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DiagPiece), 
	diagonal_piece(DiagPiece),
	colour(DiagPiece, Colour), !,
        find_diagonal_squares_direction(upright, Colour, NewRank, NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(upright, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
        NewRank is Rank - 1,
        NewFile is File + 1,
        find_diagonal_squares_direction(upright, Colour, 0, 7, ChessBoard, Tail).

find_diagonal_squares_direction(downleft, _, 7, _, _, []):- !.
find_diagonal_squares_direction(downleft, _, _, 0, _, []):- !.

find_diagonal_squares_direction(downleft, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
	NewRank is Rank + 1,
	NewFile is File - 1, 
	identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
	find_diagonal_squares_direction(downleft, Colour, NewRank, NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(downleft, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
        NewRank is Rank + 1,
        NewFile is File - 1,
        identify_piece([NewRank, NewFile], ChessBoard, DiagPiece), 
	diagonal_piece(DiagPiece),
	colour(DiagPiece, Colour), !,
        find_diagonal_squares_direction(downleft, Colour, NewRank, NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(downleft, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
	NewRank is Rank + 1,
	NewFile is File - 1,
	find_diagonal_squares_direction(downleft, Colour, 7, 0, ChessBoard, Tail).

find_diagonal_squares_direction(downright, _, 7, _, _, []):- !.
find_diagonal_squares_direction(downright, _, _, 7, _, []):- !.

find_diagonal_squares_direction(downright, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_diagonal_squares_direction(downright, Colour, NewRank, NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(downright, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DiagPiece), 
	diagonal_piece(DiagPiece), 
	colour(DiagPiece, Colour), !,
        find_diagonal_squares_direction(downright, Colour, NewRank, NewFile, ChessBoard, Tail).

find_diagonal_squares_direction(downright, Colour, Rank, File, ChessBoard, [[NewRank, NewFile]|Tail]):-
        NewRank is Rank + 1,
        NewFile is File + 1,
        find_diagonal_squares_direction(downright, Colour, 7, 7, ChessBoard, Tail).

centre_relative_position('R', Rank, 3, north):-
	Rank > 3, !. 
	
centre_relative_position('R', Rank, 4, north):-
	Rank > 3, !.

centre_relative_position('R', _, 3, south):-
	!.

centre_relative_position('R', _, 4, south):-
	!.

centre_relative_position('R', 3, File, west):-
	File > 3, !.

centre_relative_position('R', 4, File, west):-
	File > 3, !.

centre_relative_position('R', 3, _, east):-
	!.

centre_relative_position('R', 4, _, east):-
	!.

centre_relative_position('R', _, _, cannot_reach_centre).

centre_relative_position('r', Rank, 3, north):-
        Rank > 3, !.

centre_relative_position('r', Rank, 4, north):-
        Rank > 3, !.

centre_relative_position('r', _, 3, south):-
        !.

centre_relative_position('r', _, 4, south):-
        !.

centre_relative_position('r', 3, File, west):-
        File > 3, !.

centre_relative_position('r', 4, File, west):-
        File > 3, !.

centre_relative_position('r', 3, _, east):-
        !.

centre_relative_position('r', 4, _, east):-
        !.

centre_relative_position('r', _, _, cannot_reach_centre).

centre_relative_position('B', Rank, File, upright):-
	diagonal_centre_control_key_squares([Rank, File], upright), !.

centre_relative_position('B', Rank, File, upleft):-
	diagonal_centre_control_key_squares([Rank, File], upleft), !.

centre_relative_position('B', Rank, File, downright):-
	diagonal_centre_control_key_squares([Rank, File], downright), !.

centre_relative_position('B', Rank, File, downleft):-
	diagonal_centre_control_key_squares([Rank, File], downleft), !.

centre_relative_position('b', _, _, cannot_reach_centre).

centre_relative_position('b', Rank, File, upright):-
        diagonal_centre_control_key_squares([Rank, File], upright), !.

centre_relative_position('b', Rank, File, upleft):-
        diagonal_centre_control_key_squares([Rank, File], upleft), !.

centre_relative_position('b', Rank, File, downright):-
        diagonal_centre_control_key_squares([Rank, File], downright), !.

centre_relative_position('b', Rank, File, downleft):-
        diagonal_centre_control_key_squares([Rank, File], downleft), !.

centre_relative_position('b', _, _, cannot_reach_centre).  

diagonal_centre_control_key_squares([4, 3], upright).
diagonal_centre_control_key_squares([4, 2], upright).
diagonal_centre_control_key_squares([5, 1], upright).
diagonal_centre_control_key_squares([5, 2], upright).
diagonal_centre_control_key_squares([5, 3], upright).
diagonal_centre_control_key_squares([6, 0], upright).
diagonal_centre_control_key_squares([6, 1], upright).
diagonal_centre_control_key_squares([6, 2], upright).
diagonal_centre_control_key_squares([7, 0], upright).
diagonal_centre_control_key_squares([7, 1], upright).

diagonal_centre_control_key_squares([4, 4], upleft).
diagonal_centre_control_key_squares([4, 5], upleft).
diagonal_centre_control_key_squares([5, 4], upleft).
diagonal_centre_control_key_squares([5, 5], upleft).
diagonal_centre_control_key_squares([5, 6], upleft).
diagonal_centre_control_key_squares([6, 5], upleft).
diagonal_centre_control_key_squares([6, 6], upleft).
diagonal_centre_control_key_squares([6, 7], upleft).
diagonal_centre_control_key_squares([7, 6], upleft).
diagonal_centre_control_key_squares([7, 7], upleft).

diagonal_centre_control_key_squares([3, 3], downright).
diagonal_centre_control_key_squares([3, 2], downright).
diagonal_centre_control_key_squares([2, 3], downright).
diagonal_centre_control_key_squares([2, 2], downright).
diagonal_centre_control_key_squares([2, 1], downright).
diagonal_centre_control_key_squares([1, 2], downright).
diagonal_centre_control_key_squares([1, 1], downright).
diagonal_centre_control_key_squares([1, 0], downright).
diagonal_centre_control_key_squares([0, 1], downright).
diagonal_centre_control_key_squares([0, 0], downright).

diagonal_centre_control_key_squares([3, 4], downleft).
diagonal_centre_control_key_squares([3, 5], downleft).
diagonal_centre_control_key_squares([2, 4], downleft).
diagonal_centre_control_key_squares([2, 5], downleft).
diagonal_centre_control_key_squares([2, 6], downleft).
diagonal_centre_control_key_squares([1, 5], downleft).
diagonal_centre_control_key_squares([1, 6], downleft).
diagonal_centre_control_key_squares([1, 7], downleft).
diagonal_centre_control_key_squares([0, 6], downleft).
diagonal_centre_control_key_squares([0, 7], downleft).


%%%%%%%%%%%%%%% OLD STUFF %%%%%%%%%%%%%%%%%%

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

