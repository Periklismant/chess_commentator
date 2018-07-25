%%%%%%%%%%%%%%%  INCLUDES  %%%%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].
:- ['./control_squares.prolog'].


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

