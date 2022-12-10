%%%%%%%   INCLUDES   %%%%%%

:- ['chessboard_basics.prolog'].
:- ['control_squares.prolog'].

%%%%%%   FINDS THREATS   %%%%%%
identify_threats(ChessBoard, Next_to_Play, Threats):-
	identify_all_threats(0, ChessBoard, Next_to_Play, Threats_with_Fake),
        real_threats(Threats_with_Fake, Threats0),
        append(Threats0, Threats1),
        fix_threats_squares(Threats1, Threats).

identify_all_threats(8, _, _, []):- !.

identify_all_threats(Rank, ChessBoard, Next_to_Play, [Threats_on_rank|OtherThreats]):-
	identify_all_threats_rank(Rank, 0, ChessBoard, Next_to_Play, Threats_on_rank),
	NewRank is Rank + 1,
	identify_all_threats(NewRank, ChessBoard, Next_to_Play, OtherThreats).

identify_all_threats_rank(_, 8, _, _, []):- !.

identify_all_threats_rank(Rank, File, ChessBoard, Next_to_Play, [Threat_on_this_piece|OtherThreats]):-
	identify_threat_on_piece(Rank, File, ChessBoard, Next_to_Play, Threat_on_this_piece),
	NewFile is File + 1,
	identify_all_threats_rank(Rank, NewFile, ChessBoard, Next_to_Play, OtherThreats).

identify_threat_on_piece(Rank, File, ChessBoard, _, [Rank, File, 0]):-
	identify_piece([Rank,File], ChessBoard, ' '), !.

identify_threat_on_piece(Rank, File, ChessBoard, Colour, [Rank, File, 0]):-
	identify_piece([Rank, File], ChessBoard, Piece),
	colour(Piece, PieceColour),
	reverse_colour(Colour, PieceColour),!.

identify_threat_on_piece(Rank, File, ChessBoard, Colour, Threats):-
	identify_piece([Rank, File], ChessBoard, Piece),
	evaluate(Piece, Evaluation),
	target_piece(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking),
	pieces_to_evaluations(PiecesDefending, PiecesDefendingEvals),
	pieces_to_evaluations(PiecesAttacking, PiecesAttackingEvals),
	predsort(nthcompare(1), PiecesDefendingEvals, PiecesDefendingEvalsSorted),
	predsort(nthcompare(1), PiecesAttackingEvals, PiecesAttackingEvalsSorted),!,
	check_for_threat(Rank, File, Evaluation, PiecesDefendingEvalsSorted, PiecesAttackingEvalsSorted, 0, Threats).

real_threats([],[]).

real_threats([H|T],Threats):-
	real_threats_rank(H, []),!,
	real_threats(T, Threats).

real_threats([H|T], [Threats_on_this_rank|OtherThreats]):-
	real_threats_rank(H, Threats_on_this_rank),
	real_threats(T, OtherThreats).

real_threats_rank([], []).

real_threats_rank([[_,_,0]|T], Threats):-
	!, real_threats_rank(T, Threats).

real_threats_rank([[_, _, Neg]|T], Threats):-
	Neg < 0, !, real_threats_rank(T, Threats).

real_threats_rank([H|T], [H|OtherThreats]):-
	real_threats_rank(T, OtherThreats).	

fix_threats_squares([],[]).

fix_threats_squares([[Rank, File, Score]|T],[[Square, Score]|Tail]):-
	index_to_square([Rank,File], Square),
	fix_threats_squares(T, Tail).

check_for_threat(Rank, File, _, _, [], Score, [Rank, File, Score]):-
	Score > 0, !.

check_for_threat(Rank, File, _, _, [], _, [Rank, File, 0]):- !.

check_for_threat(Rank, File, 1000, _, _, _, [Rank, File, check]):- !.

check_for_threat(Rank, File, Evaluation, [], _, ScoreTemp, [Rank, File, Score]):- 
	Score is ScoreTemp + Evaluation,
	Score > 0, !.

check_for_threat(Rank, File,  _, [], _, _, [Rank, File, 0]):- !.

check_for_threat(Rank, File, Evaluation, [[DefenderEval, _, _]|_], [[AttackerEval, _, _]|_], Score, [Rank, File, Score]):-
        SumDefendingEval is DefenderEval + Evaluation,
        AttackerEval > SumDefendingEval,
	Score > 0, !.

check_for_threat(Rank, File, Evaluation, [[DefenderEval, _, _]|_], [[AttackerEval, _, _]|_], _, [Rank, File, 0]):-
	SumDefendingEval is DefenderEval + Evaluation,
	AttackerEval > SumDefendingEval, !.

check_for_threat(Rank, File, Evaluation, [[1000, _, _]|_], [[AttackerEval, _, _]|[]], ScoreTemp, [Rank, File, Score]):-
	Evaluation > AttackerEval, !,
	Score is ScoreTemp + Evaluation - AttackerEval.

check_for_threat(Rank, File, _, [[1000, _, _]|_], [_|[]], ScoreTemp, [Rank, File, ScoreTemp]):- 
	ScoreTemp > 0, !.

check_for_threat(Rank, File, _, [[1000, _, _]|_], [_|[]], _, [Rank, File, 0]):- !.

check_for_threat(Rank, File, Evaluation, [[1000, _, _]|_], [_|[_|_]], ScoreTemp, [Rank, File, Score]):-
	!, Score is ScoreTemp + Evaluation.

check_for_threat(Rank, File, Evaluation, [[DefenderEval, _, _]|OtherDefenders], [[AttackerEval, _, _]|OtherAttackers], ScoreTemp, [Rank, File, ExpectedTakeBackScore]):-
	NoTakeBackScore is ScoreTemp + Evaluation,
	check_for_threat(Rank, File, DefenderEval, OtherDefenders, OtherAttackers, 0, [Rank, File, ScoreOnNextDefender]),
	ExpectedTakeBackScore is ScoreTemp + Evaluation - AttackerEval + ScoreOnNextDefender,
	ExpectedTakeBackScore < NoTakeBackScore, !.

check_for_threat(Rank, File, Evaluation, _, _, ScoreTemp, [Rank, File, NoTakeBackScore]):-
	NoTakeBackScore is ScoreTemp + Evaluation,  !.

target_piece(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
	kings_attacking_defending(Colour, Rank, File, ChessBoard, PiecesDefendingKings, PiecesAttackingKings),
	line_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefendingLine, PiecesAttackingLine),
	diagonal_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefendingDiag, PiecesAttackingDiag),
	knights_attacking_defending(Colour, Rank, File, ChessBoard, DefendedbyKnights, AttackedbyKnights),
	pawns_attacking_defending(Colour, Rank, File, ChessBoard, PiecesDefendingPawns, PiecesAttackingPawns),
	fix_knights_attacking_defending(Colour, DefendedbyKnights, AttackedbyKnights, PiecesDefendingKnights, PiecesAttackingKnights),
	append([PiecesDefendingKings, PiecesDefendingKnights, PiecesDefendingPawns, PiecesDefendingLine, PiecesDefendingDiag], PiecesDefending),
	append([PiecesAttackingKings, PiecesAttackingKnights, PiecesAttackingPawns, PiecesAttackingLine, PiecesAttackingDiag], PiecesAttacking).

%%%%%%  FIX  %%%%%%

fix_knights_attacking_defending(_, [], [], [], []):- !.

fix_knights_attacking_defending(white, [], [[Rank, File]|T], DefPieces, [['n', Rank, File]|Tail]):-
	!,
	fix_knights_attacking_defending(white, [], T, DefPieces, Tail).

fix_knights_attacking_defending(black, [], [[Rank, File]|T], DefPieces, [['N', Rank, File]|Tail]):-
	!,
	fix_knights_attacking_defending(white, [], T, DefPieces, Tail).

fix_knights_attacking_defending(white, [[Rank, File]|T], Att, [['N', Rank, File]|Tail], AttPieces):-
	fix_knights_attacking_defending(white, T, Att, Tail, AttPieces).

fix_knights_attacking_defending(black, [[Rank, File]|T], Att, [['n', Rank, File]|Tail], AttPieces):-
	fix_knights_attacking_defending(white, T, Att, Tail, AttPieces).

%%%%%%  PAWNS    %%%%%%

pawns_attacking_defending(white, 0, 0, ChessBoard, DefendedbyPawns, []):-
	!, identify_piece([1,1], ChessBoard, Piece),
	count_pawns([[Piece, 1, 1]], DefendedbyPawns, _).

pawns_attacking_defending(white, 0, 7, ChessBoard, DefendedbyPawns, []):-
	!, identify_piece([1,6], ChessBoard, Piece),
	count_pawns([[Piece, 1, 6]], DefendedbyPawns, _).

pawns_attacking_defending(white, 7, 0, ChessBoard, [], AttackedbyPawns):-
	!, identify_piece([6,1], ChessBoard, Piece),
	count_pawns([[Piece, 6, 1]], _, AttackedbyPawns).

pawns_attacking_defending(white, 7, 7, ChessBoard, [], AttackedbyPawns):-
	!, identify_piece([6,6], ChessBoard, Piece),
	count_pawns([[Piece, 6, 6]], _, AttackedbyPawns).

pawns_attacking_defending(black, 0, 0, ChessBoard, [], AttackedbyPawns):-
        !, identify_piece([1,1], ChessBoard, Piece),
        count_pawns([[Piece, 1, 1]], AttackedbyPawns, _).

pawns_attacking_defending(black, 0, 7, ChessBoard, [], AttackedbyPawns):-
        !, identify_piece([1,6], ChessBoard, Piece),
        count_pawns([[Piece, 1, 6]], AttackedbyPawns, _).

pawns_attacking_defending(black, 7, 0, ChessBoard, DefendedbyPawns, []):-
        !, identify_piece([6,1], ChessBoard, Piece),
        count_pawns([[Piece, 6, 1]], _, DefendedbyPawns).

pawns_attacking_defending(black, 7, 7, ChessBoard, DefendedbyPawns, []):-
        !, identify_piece([6,6], ChessBoard, Piece),
        count_pawns([[Piece, 6, 6]], _, DefendedbyPawns).

pawns_attacking_defending(white, 0, File, ChessBoard, DefendedbyPawns, []):-
	!, NewFile1 is File - 1,
	NewFile2 is File + 1,
	identify_piece([1, NewFile1], ChessBoard, Piece1), 
	identify_piece([1, NewFile2], ChessBoard, Piece2),
	count_pawns([[Piece1, 1, NewFile1], [Piece2, 1, NewFile2]], DefendedbyPawns, _).
	
pawns_attacking_defending(white, 7, File, ChessBoard, [], AttackedbyPawns):-
	!, NewFile1 is File - 1,
	NewFile2 is File + 1,
	identify_piece([6, NewFile1], ChessBoard, Piece1),
	identify_piece([6, NewFile2], ChessBoard, Piece2),
	count_pawns([[Piece1, 6, NewFile1], [Piece2, 6, NewFile2]], _, AttackedbyPawns).

pawns_attacking_defending(white, Rank, 0, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
	!, NewRank1 is Rank - 1,
	NewRank2 is Rank + 1,
	identify_piece([NewRank1, 1], ChessBoard, Piece1),
	count_pawns([[Piece1, NewRank1, 1]], _, AttackedbyPawns),
	identify_piece([NewRank2, 1], ChessBoard, Piece2),
	count_pawns([[Piece2, NewRank2, 1]], DefendedbyPawns, _).

pawns_attacking_defending(white, Rank, 7, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
	!, NewRank1 is Rank - 1,
	NewRank2 is Rank + 1,
	identify_piece([NewRank1, 6], ChessBoard, Piece1),
	count_pawns([[Piece1, NewRank1, 6]], _, AttackedbyPawns),
	identify_piece([NewRank2, 6], ChessBoard, Piece2),
	count_pawns([[Piece2, NewRank2, 6]], DefendedbyPawns, _).

pawns_attacking_defending(black, 0, File, ChessBoard, [], AttackedbyPawns):-
        !, NewFile1 is File - 1,
        NewFile2 is File + 1,
        identify_piece([1, NewFile1], ChessBoard, Piece1),
        identify_piece([1, NewFile2], ChessBoard, Piece2),
        count_pawns([[Piece1, 1, NewFile1], [Piece2, 1, NewFile2]], AttackedbyPawns, _).

pawns_attacking_defending(black, 7, File, ChessBoard, DefendedbyPawns, []):-
        !, NewFile1 is File - 1,
        NewFile2 is File + 1,
        identify_piece([6, NewFile1], ChessBoard, Piece1),
        identify_piece([6, NewFile2], ChessBoard, Piece2),
        count_pawns([[Piece1, 6, NewFile1], [Piece2, 6, NewFile2]], _, DefendedbyPawns).

pawns_attacking_defending(black, Rank, 0, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
        !, NewRank1 is Rank - 1,
        NewRank2 is Rank + 1,
        identify_piece([NewRank1, 1], ChessBoard, Piece1),
        count_pawns([[Piece1, NewRank1, 1]], _, DefendedbyPawns),
        identify_piece([NewRank2, 1], ChessBoard, Piece2),
        count_pawns([[Piece2, NewRank2, 1]], AttackedbyPawns, _).

pawns_attacking_defending(black, Rank, 7, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
        !, NewRank1 is Rank - 1,
        NewRank2 is Rank + 1,
        identify_piece([NewRank1, 6], ChessBoard, Piece1),
        count_pawns([[Piece1, NewRank1, 6]], _, DefendedbyPawns),
        identify_piece([NewRank2, 6], ChessBoard, Piece2),
        count_pawns([[Piece2, NewRank2, 6]], AttackedbyPawns, _).

pawns_attacking_defending(white, Rank, File, ChessBoard, DefendedbyPawns,AttackedbyPawns):-
	RankMinus is Rank - 1, 
	RankPlus is Rank + 1,
	FileMinus is File - 1,
	FilePlus is File + 1,
	identify_piece([RankMinus, FileMinus], ChessBoard, Piece1), 	
	identify_piece([RankMinus, FilePlus], ChessBoard, Piece2), 	
	identify_piece([RankPlus, FileMinus], ChessBoard, Piece3), 	
	identify_piece([RankPlus, FilePlus], ChessBoard, Piece4), 
	count_pawns([[Piece1, RankMinus, FileMinus], [Piece2, RankMinus, FilePlus]], _, AttackedbyPawns),
	count_pawns([[Piece3, RankPlus, FileMinus], [Piece4, RankPlus, FilePlus]], DefendedbyPawns, _).

pawns_attacking_defending(black, Rank, File, ChessBoard, DefendedbyPawns,AttackedbyPawns):-
        RankMinus is Rank - 1,   
        RankPlus is Rank + 1,
        FileMinus is File - 1,
        FilePlus is File + 1,
        identify_piece([RankMinus, FileMinus], ChessBoard, Piece1),     
        identify_piece([RankMinus, FilePlus], ChessBoard, Piece2), 
        identify_piece([RankPlus, FileMinus], ChessBoard, Piece3), 
        identify_piece([RankPlus, FilePlus], ChessBoard, Piece4),  
        count_pawns([[Piece1, RankMinus, FileMinus], [Piece2, RankMinus, FilePlus]], _, DefendedbyPawns),
        count_pawns([[Piece3, RankPlus, FileMinus], [Piece4, RankPlus, FilePlus]], AttackedbyPawns, _).

count_pawns([], [], []).

count_pawns([['p', Rank, File]|T], WhitePawns, [['p', Rank, File]|Tail]):- 
	!, 
	count_pawns(T, WhitePawns, Tail).

count_pawns([['P', Rank, File]|T], [['P', Rank, File]|Tail], BlackPawns):-
	!, 
	count_pawns(T, Tail, BlackPawns).

count_pawns([_|T], WhitePawns, BlackPawns):-
	count_pawns(T, WhitePawns, BlackPawns).
	
	
%%%%%%  KNIGHTS  %%%%%%% 

knights_attacking_defending(Colour, Rank, File, ChessBoard, DefendedbyKnights, AttackedbyKnights):-
	oneOClock_knight(Colour, Rank, File, ChessBoard, DefOneOClock, AttOneOClock), 
	twoOClock_knight(Colour, Rank, File, ChessBoard, DefTwoOClock, AttTwoOClock), 
	fourOClock_knight(Colour, Rank, File, ChessBoard, DefFourOClock, AttFourOClock), 
	fiveOClock_knight(Colour, Rank, File, ChessBoard, DefFiveOClock, AttFiveOClock), 
	sevenOClock_knight(Colour, Rank, File, ChessBoard, DefSevenOClock, AttSevenOClock),
	eightOClock_knight(Colour, Rank, File, ChessBoard, DefEightOClock, AttEightOClock),
	tenOClock_knight(Colour, Rank, File, ChessBoard, DefTenOClock, AttTenOClock),
	elevenOClock_knight(Colour, Rank, File, ChessBoard, DefElevenOClock, AttElevenOClock),
	append([DefOneOClock, DefTwoOClock, DefFourOClock, DefFiveOClock, DefSevenOClock, DefEightOClock, DefTenOClock, DefElevenOClock], DefendedbyKnights),
	append([AttOneOClock, AttTwoOClock, AttFourOClock, AttFiveOClock, AttSevenOClock, AttEightOClock, AttTenOClock, AttElevenOClock], AttackedbyKnights).
	
elevenOClock_knight(_, 0, _, _, [], []):- !.
elevenOClock_knight(_, 1, _, _, [], []):- !.
elevenOClock_knight(_, _, 0, _, [], []):- !.

elevenOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
        TargetRank is Rank - 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

elevenOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank - 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

elevenOClock_knight(_, _, _, _, [], []).

tenOClock_knight(_, 0, _, _, [], []):- !.
tenOClock_knight(_, _, 0, _, [], []):- !.
tenOClock_knight(_, _, 1, _, [], []):- !.

tenOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
        TargetRank is Rank - 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

tenOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank - 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

tenOClock_knight(_, _, _, _, [], []).

eightOClock_knight(_, 7, _, _, [], []):- !.
eightOClock_knight(_, _, 0, _, [], []):- !.
eightOClock_knight(_, _, 1, _, [], []):- !.

eightOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
        TargetRank is Rank + 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

eightOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank + 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

eightOClock_knight(_, _, _, _, [], []).

sevenOClock_knight(_, 6, _, _, [], []):- !.
sevenOClock_knight(_, 7, _, _, [], []):- !.
sevenOClock_knight(_, _, 0, _, [], []):- !.

sevenOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
        TargetRank is Rank + 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

sevenOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank + 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

sevenOClock_knight(_, _, _, _, [], []).

fiveOClock_knight(_, 6, _, _, [], []):- !.
fiveOClock_knight(_, 7, _, _, [], []):- !.
fiveOClock_knight(_, _, 7, _, [], []):- !.

fiveOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
        TargetRank is Rank + 2,
        TargetFile is File + 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

fiveOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank + 2,
        TargetFile is File + 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

fiveOClock_knight(_, _, _, _, [], []).

fourOClock_knight(_, 7, _, _, [], []):- !.
fourOClock_knight(_, _, 6, _, [], []):- !.
fourOClock_knight(_, _, 7, _, [], []):- !.

fourOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
        TargetRank is Rank + 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

fourOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank + 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

fourOClock_knight(_, _, _, _, [], []).

twoOClock_knight(_, 0, _, _, [], []):- !.
twoOClock_knight(_, _, 6, _, [], []):- !.
twoOClock_knight(_, _, 7, _, [], []):- !.

twoOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
        TargetRank is Rank - 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

twoOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank - 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour), 
        reverse_colour(AttColour, Colour), !.

twoOClock_knight(_, _, _, _, [], []).

oneOClock_knight(_, 0, _, _, [], []):- !.
oneOClock_knight(_, 1, _, _, [], []):- !.
oneOClock_knight(_, _, 7, _, [], []):- !.

oneOClock_knight(Colour, Rank, File, ChessBoard, [[TargetRank, TargetFile]], []):-
	TargetRank is Rank - 2,
	TargetFile is File + 1,
	identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
	knight_piece(DefPiece),
	colour(DefPiece, Colour), !.

oneOClock_knight(Colour, Rank, File, ChessBoard, [], [[TargetRank, TargetFile]]):-
        TargetRank is Rank - 2,
        TargetFile is File + 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour), 
	reverse_colour(AttColour, Colour), !.

oneOClock_knight(_, _, _, _, [], []).
	

%%%%%%  DIAGONAL PIECES  %%%%%%%

diagonal_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
	upleft_attackers_defenders(Colour, Rank, File, ChessBoard, UpleftPiecesDefending, UpleftPiecesAttacking),
	upright_attackers_defenders(Colour, Rank, File, ChessBoard, UprightPiecesDefending, UprightPiecesAttacking),
	downleft_attackers_defenders(Colour, Rank, File, ChessBoard, DownleftPiecesDefending, DownleftPiecesAttacking),
	downright_attackers_defenders(Colour, Rank, File, ChessBoard, DownrightPiecesDefending, DownrightPiecesAttacking),
	append([UpleftPiecesDefending, UprightPiecesDefending, DownleftPiecesDefending, DownrightPiecesDefending], PiecesDefending),
	append([UpleftPiecesAttacking, UprightPiecesAttacking, DownleftPiecesAttacking, DownrightPiecesAttacking], PiecesAttacking).	

downright_attackers_defenders(_, 7, _, _, [], []):- !.
downright_attackers_defenders(_, _, 7, _, [], []):- !.

downright_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        downright_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, PiecesDefending, PiecesAttacking).

downright_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, NewRank, NewFile]|Tail]):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

downright_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, NewRank, NewFile]|Tail], []):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece), !,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, defending_battery, Tail).

downright_attackers_defenders(_, _, _, _, [], []).

downleft_attackers_defenders(_, 7, _, _, [], []):- !.
downleft_attackers_defenders(_, _, 0, _, [], []):- !.

downleft_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        downleft_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, PiecesDefending, PiecesAttacking).

downleft_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, Rank, File]|Tail]):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

downleft_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, Rank, File]|Tail], []):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece), !,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, defending_battery, Tail).

downleft_attackers_defenders(_, _, _, _, [], []).

upright_attackers_defenders(_, 0, _, _, [], []):- !.
upright_attackers_defenders(_, _, 7, _, [], []):- !.

upright_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        upright_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, PiecesDefending, PiecesAttacking).

upright_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, NewRank, NewFile]|Tail]):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

upright_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, NewRank, NewFile]|Tail], []):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece), !,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, defending_battery, Tail).

upright_attackers_defenders(_, _, _, _, [], []).


upleft_attackers_defenders(_, 0, _, _, [], []):- !.
upleft_attackers_defenders(_, _, 0, _, [], []):- !.

upleft_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
	NewFile is File - 1, 
	NewRank is Rank - 1,
	identify_piece([NewRank, NewFile], ChessBoard, ' '), !, 
	upleft_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, PiecesDefending, PiecesAttacking).

upleft_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, NewRank, NewFile]|Tail]):-
	NewFile is File - 1,
	NewRank is Rank - 1,
	identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
	diagonal_piece(AttPiece),
	colour(AttPiece, AttColour),
	reverse_colour(Colour, AttColour), !,
	find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

upleft_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, NewRank, NewFile]|Tail], []):-
	NewFile is File - 1,
	NewRank is Rank - 1,
	identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
	diagonal_piece(DefPiece), !,
	find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, defending_battery, Tail).

upleft_attackers_defenders(_, _, _, _, [], []).

%%%%%%  LINE PIECES  %%%%%%%   %%% FIX BATTERY MAYBE?? -- SAME AS DIAG %%%

line_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):- 
	north_attackers_defenders(Colour, Rank, File, ChessBoard, NorthPiecesDefending, NorthPiecesAttacking),
	south_attackers_defenders(Colour, Rank, File, ChessBoard, SouthPiecesDefending, SouthPiecesAttacking),
	west_attackers_defenders(Colour, Rank, File, ChessBoard, WestPiecesDefending, WestPiecesAttacking),
	east_attackers_defenders(Colour, Rank, File, ChessBoard, EastPiecesDefending, EastPiecesAttacking),
	append([NorthPiecesDefending, SouthPiecesDefending, WestPiecesDefending, EastPiecesDefending], PiecesDefending),
	append([NorthPiecesAttacking, SouthPiecesAttacking, WestPiecesAttacking, EastPiecesAttacking], PiecesAttacking).
	
east_attackers_defenders(_, _, 7, _, [], []):- !.

east_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        east_attackers_defenders(Colour, Rank, NewFile, ChessBoard, PiecesDefending, PiecesAttacking).

east_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, Rank, NewFile]|Tail]):-
      	NewFile is File + 1,
	identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, attacking_battery, Tail).

east_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, Rank, NewFile]|Tail], []):-
       	NewFile is File + 1,
	identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece), !,
	find_battery(Colour, east, Rank, NewFile, ChessBoard, defending_battery, Tail).

east_attackers_defenders(_, _, _, _, [], []).

west_attackers_defenders(_, _, 0, _, [], []):- !.

west_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
        NewFile is File - 1,
	identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        west_attackers_defenders(Colour, Rank, NewFile, ChessBoard, PiecesDefending, PiecesAttacking).

west_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, Rank, NewFile]|Tail]):-
        NewFile is File - 1,
	identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !, 
	find_battery(Colour, west, Rank, NewFile, ChessBoard, attacking_battery, Tail).

west_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, Rank, NewFile]|Tail], []):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece), !,
	find_battery(Colour, west, Rank, NewFile, ChessBoard, defending_battery, Tail).
	
west_attackers_defenders(_, _, _, _, [], []).

south_attackers_defenders(_, 7, _, _, [], []):- !.

south_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
	NewRank is Rank + 1,
	identify_piece([NewRank, File], ChessBoard, ' '), !, 
	south_attackers_defenders(Colour, NewRank, File, ChessBoard, PiecesDefending, PiecesAttacking).

south_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, NewRank, File]|Tail]):-
	NewRank is Rank + 1,
	identify_piece([NewRank, File], ChessBoard, AttPiece), 
	line_piece(AttPiece),
	colour(AttPiece, AttColour),
	reverse_colour(Colour, AttColour), !,
	find_battery(Colour, south, NewRank, File, ChessBoard, attacking_battery, Tail).

south_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, NewRank, File]|Tail], []):-
	NewRank is Rank + 1,
	identify_piece([NewRank, File], ChessBoard, DefPiece),
	line_piece(DefPiece), !,
	find_battery(Colour, south, NewRank, File, ChessBoard, defending_battery, Tail).

south_attackers_defenders(_, _, _, _, [], []). 
		
north_attackers_defenders(_, 0, _, _, [], []):- !.

north_attackers_defenders(Colour, Rank, File, ChessBoard, PiecesDefending, PiecesAttacking):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, ' '), !,
        north_attackers_defenders(Colour, NewRank, File, ChessBoard, PiecesDefending, PiecesAttacking).

north_attackers_defenders(Colour, Rank, File, ChessBoard, [], [[AttPiece, NewRank, File]|Tail]):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
	find_battery(Colour, north, NewRank, File, ChessBoard, attacking_battery, Tail).
	
north_attackers_defenders(Colour, Rank, File, ChessBoard, [[DefPiece, NewRank, File]|Tail], []):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, DefPiece),
        line_piece(DefPiece), !,
	find_battery(Colour, north, NewRank, File, ChessBoard, defending_battery, Tail).

north_attackers_defenders(_, _, _, _, [], []).    

find_battery(_, east, _, 7, _, _, []):- !.
find_battery(_, west, _, 0, _, _, []):- !.
find_battery(_, north, 0, _, _, _, []):- !.
find_battery(_, south, 7, _, _, _, []):- !.

find_battery(_, upleft, 0, _, _, _, []):- !.
find_battery(_, upleft, _, 0, _, _, []):- !.
find_battery(_, upright, 0, _, _, _, []):- !.
find_battery(_, upright, _, 7, _, _, []):- !.
find_battery(_, downleft, 7, _, _, _, []):- !.
find_battery(_, downleft, _, 0, _, _, []):- !.
find_battery(_, downright, 7, _, _, _, []):- !.
find_battery(_, downright, _, 7, _, _, []):- !.

find_battery(Colour, downright, Rank, File, ChessBoard, AttackorDefence, Pieces):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, AttackorDefence, Pieces).

find_battery(Colour, downright, Rank, File, ChessBoard, attacking_battery, [[AttPiece, NewRank, NewFile]|Tail]):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

find_battery(Colour, downright, Rank, File, ChessBoard, defending_battery, [[DefPiece, NewRank, NewFile]|Tail]):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, defending_battery, Tail).

find_battery(_, downright, _, _, _, _, []).

find_battery(Colour, downleft, Rank, File, ChessBoard, AttackorDefence, Pieces):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, AttackorDefence, Pieces).

find_battery(Colour, downleft, Rank, File, ChessBoard, attacking_battery, [[AttPiece, NewRank, NewFile]|Tail]):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

find_battery(Colour, downleft, Rank, File, ChessBoard, defending_battery, [[DefPiece, NewRank, NewFile]|Tail]):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, defending_battery, Tail).

find_battery(_, downleft, _, _, _, _, []).

find_battery(Colour, upleft, Rank, File, ChessBoard, AttackorDefence, Pieces):-
        NewFile is File - 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, AttackorDefence, Pieces).

find_battery(Colour, upleft, Rank, File, ChessBoard, attacking_battery, [[AttPiece, NewRank, NewFile]|Tail]):-
        NewFile is File - 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

find_battery(Colour, upleft, Rank, File, ChessBoard, defending_battery, [[DefPiece, NewRank, NewFile]|Tail]):-
        NewFile is File - 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, defending_battery, Tail).

find_battery(_, upleft, _, _, _, _, []).

find_battery(Colour, upright, Rank, File, ChessBoard, AttackorDefence, Pieces):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, AttackorDefence, Pieces).

find_battery(Colour, upright, Rank, File, ChessBoard, attacking_battery, [[AttPiece, NewRank, NewFile]|Tail]):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, attacking_battery, Tail).

find_battery(Colour, upright, Rank, File, ChessBoard, defending_battery, [[DefPiece, NewRank, NewFile]|Tail]):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, defending_battery, Tail).

find_battery(_, upright, _, _, _, _, []).

find_battery(Colour, east, Rank, File, ChessBoard, AttackOrDefence, Pieces):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, AttackOrDefence, Pieces).

find_battery(Colour, east, Rank, File, ChessBoard, attacking_battery, [[AttPiece, Rank, NewFile] | Tail]):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, attacking_battery, Tail).

find_battery(Colour, east, Rank, File, ChessBoard, defending_battery, [[DefPiece, Rank, NewFile] | Tail]):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, defending_battery, Tail).

find_battery(_, east, _, _, _, _, []).

find_battery(Colour, west, Rank, File, ChessBoard, AttackOrDefence, Pieces):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, west, Rank, NewFile, ChessBoard, AttackOrDefence, Pieces).

find_battery(Colour, west, Rank, File, ChessBoard, attacking_battery, [[AttPiece, Rank, NewFile] | Tail]):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, west, Rank, NewFile, ChessBoard, attacking_battery, Tail).

find_battery(Colour, west, Rank, File, ChessBoard, defending_battery, [[DefPiece, Rank, NewFile] | Tail]):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, west, Rank, NewFile, ChessBoard, defending_battery, Tail).

find_battery(_, west, _, _, _, _, []).

find_battery(Colour, north, Rank, File, ChessBoard, AttackOrDefence, Pieces):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, ' '), !,
        find_battery(Colour, north, NewRank, File, ChessBoard, AttackOrDefence, Pieces).

find_battery(Colour, north, Rank, File, ChessBoard, attacking_battery, [[AttPiece, NewRank, File] | Tail]):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, north, NewRank, File, ChessBoard, attacking_battery, Tail).

find_battery(Colour, north, Rank, File, ChessBoard, defending_battery, [[DefPiece, NewRank, File] | Tail]):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, north, NewRank, File, ChessBoard, defending_battery, Tail).

find_battery(_, north, _, _, _, _, []).

find_battery(Colour, south, Rank, File, ChessBoard, AttackOrDefence, Pieces):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], ChessBoard, ' '), !,
        find_battery(Colour, south, NewRank, File, ChessBoard, AttackOrDefence, Pieces).

find_battery(Colour, south, Rank, File, ChessBoard, attacking_battery, [[AttPiece, NewRank, File] | Tail]):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        find_battery(Colour, south, NewRank, File, ChessBoard, attacking_battery, Tail).

find_battery(Colour, south, Rank, File, ChessBoard, defending_battery, [[DefPiece, NewRank, File] | Tail]):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        find_battery(Colour, south, NewRank, File, ChessBoard, defending_battery, Tail).

find_battery(_, south, _, _, _, _, []).


%%%%%%  KINGS  %%%%%%%

kings_attacking_defending(Colour, 0, 0, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
	find_kings_on_squares(Colour, [[0,1],[1,0],[1,1]], ChessBoard, DefendedbyKing, AttackedbyKing). 
	
kings_attacking_defending(Colour, 0, 7, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
	find_kings_on_squares(Colour, [[0,6],[1,6],[1,7]], ChessBoard, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 7, 0, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
        find_kings_on_squares(Colour, [[6,0],[6,1],[7,1]], ChessBoard, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 7, 7, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
        find_kings_on_squares(Colour, [[7,6],[6,7],[6,6]], ChessBoard, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 0, File, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
	FileMinus is File - 1, 
	FilePlus is File + 1,
	find_kings_on_squares(Colour, [[0,FileMinus],[0,FilePlus],[1,FileMinus],[1,File],[1,FilePlus]], ChessBoard, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 7, File, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
        FileMinus is File - 1,
        FilePlus is File + 1,
        find_kings_on_squares(Colour, [[7,FileMinus],[7,FilePlus],[6,FileMinus],[6,File],[6,FilePlus]], ChessBoard, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, Rank, 0, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
        RankMinus is Rank - 1,
        RankPlus is Rank + 1, 
        find_kings_on_squares(Colour, [[RankMinus,0],[RankPlus,0],[RankMinus,1],[Rank,1],[RankPlus,1]], ChessBoard, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, Rank, 7, ChessBoard, DefendedbyKing, AttackedbyKing):- !,
        RankMinus is Rank - 1, 
        RankPlus is Rank + 1,
        find_kings_on_squares(Colour, [[RankMinus,7],[RankPlus,7],[RankMinus,6],[Rank,6],[RankPlus,6]], ChessBoard, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, Rank, File, ChessBoard, DefendedbyKing, AttackedbyKing):-
	RankMinus is Rank - 1,
        RankPlus is Rank + 1,
	FileMinus is File - 1,
        FilePlus is File + 1,
	find_kings_on_squares(Colour, [[RankMinus,FileMinus], [RankMinus, File], [RankMinus, FilePlus], [Rank, FileMinus], [Rank, FilePlus], [RankPlus, FileMinus], [RankPlus, File], [RankPlus, FilePlus]], ChessBoard, DefendedbyKing, AttackedbyKing).
	
find_kings_on_squares(_, [], _, [], []):- !.

find_kings_on_squares(white, [[Rank, File]|T], ChessBoard, [['K', Rank, File]|Tail], AttackedbyKing):-
	identify_piece([Rank,File], ChessBoard, 'K'), !, 
	find_kings_on_squares(white, T, ChessBoard, Tail, AttackedbyKing). 

find_kings_on_squares(white, [[Rank, File]|T], ChessBoard, DefendedbyKing, [['k', Rank, File]|Tail]):-
	identify_piece([Rank, File], ChessBoard, 'k'), !,
	find_kings_on_squares(white, T, ChessBoard, DefendedbyKing, Tail).

find_kings_on_squares(white, [_|T], ChessBoard, DefendedbyKing, AttackedbyKing):-
	find_kings_on_squares(white, T, ChessBoard, DefendedbyKing, AttackedbyKing).

find_kings_on_squares(black, [[Rank, File]|T], ChessBoard, [['k', Rank, File]|Tail], AttackedbyKing):-
	identify_piece([Rank, File], ChessBoard, 'k'), !,
	find_kings_on_squares(black, T, ChessBoard, Tail, AttackedbyKing).

find_kings_on_squares(black, [[Rank, File]|T], ChessBoard, DefendedbyKing, [['K', Rank, File]|Tail]):-
	identify_piece([Rank, File], ChessBoard, 'K'), !,
	find_kings_on_squares(black, T, ChessBoard, DefendedbyKing, Tail).

find_kings_on_squares(black, [_|T], ChessBoard, DefendedbyKing, AttackedbyKing):-
        find_kings_on_squares(black, T, ChessBoard, DefendedbyKing, AttackedbyKing).

