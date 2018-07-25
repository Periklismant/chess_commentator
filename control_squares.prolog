%%%%%%%%%%%%%%%%%%%%%%%     INCLUDE      %%%%%%%%%%%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- discontiguous piece_control_squares/4.
:- discontiguous remove_blocked_squares/7.

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

