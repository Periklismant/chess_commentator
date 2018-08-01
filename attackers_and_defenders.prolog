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
	target_piece(Colour, Rank, File, ChessBoard, _DefendedTimes, PiecesDefending, _AttackedTimes, PiecesAttacking),
	pieces_to_evaluations(PiecesDefending, PiecesDefendingEvals),
	pieces_to_evaluations(PiecesAttacking, PiecesAttackingEvals),
	msort(PiecesDefendingEvals, PiecesDefendingEvalsSorted),
	msort(PiecesAttackingEvals, PiecesAttackingEvalsSorted),
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

real_threats_rank([H|T], [H|OtherThreats]):-
	real_threats_rank(T, OtherThreats).	

fix_threats_squares([],[]).

fix_threats_squares([[Rank, File, Score]|T],[[Square, Score]|Tail]):-
	index_to_square([Rank,File], Square),
	fix_threats_squares(T, Tail).

%%%%%%  KINGS FIX!!!!!!!!!  %%%%%%
check_for_threat(Rank, File, _, _, [], Score, [Rank, File, Score]):-
	Score > 0, !.

check_for_threat(Rank, File, _, _, [], _, [Rank, File, 0]):- !.

check_for_threat(Rank, File, 1000, _, _, _, [Rank, File, check]):- !.

check_for_threat(Rank, File, Evaluation, [], _, ScoreTemp, [Rank, File, Score]):- 
	Score is ScoreTemp + Evaluation,
	Score > 0, !.

check_for_threat(Rank, File,  _, [], _, _, [Rank, File, 0]):- !.

check_for_threat(Rank, File, Evaluation, [DefenderEval|_], [AttackerEval|_], Score, [Rank, File, Score]):-
        SumDefendingEval is DefenderEval + Evaluation,
        AttackerEval > SumDefendingEval,
	Score > 0, !.

check_for_threat(Rank, File, Evaluation, [DefenderEval|_], [AttackerEval|_], _, [Rank, File, 0]):-
	SumDefendingEval is DefenderEval + Evaluation,
	AttackerEval > SumDefendingEval, !.

check_for_threat(Rank, File, Evaluation, [1000|_], [AttackerEval|[]], ScoreTemp, [Rank, File, Score]):-
	Evaluation > AttackerEval, !,
	Score is ScoreTemp + Evaluation - AttackerEval.

check_for_threat(Rank, File, _, [1000|_], [_|[]], ScoreTemp, [Rank, File, ScoreTemp]):- 
	ScoreTemp > 0, !.

check_for_threat(Rank, File, _, [1000|_], [_|[]], _, [Rank, File, 0]):- !.

check_for_threat(Rank, File, Evaluation, [1000|_], [_|[_|_]], ScoreTemp, [Rank, File, Score]):-
	!, Score is ScoreTemp + Evaluation.

check_for_threat(Rank, File, Evaluation, [DefenderEval|OtherDefenders], [AttackerEval|OtherAttackers], ScoreTemp, [Rank, File, NewScore]):-
	Evaluation > AttackerEval,
	NewScore is ScoreTemp + Evaluation - AttackerEval,
	check_for_threat(Rank, File, DefenderEval, OtherDefenders, OtherAttackers, 0, [Rank, File, Score]),
	Score < AttackerEval, !.

check_for_threat(Rank, File, Evaluation, _, [AttackerEval|_], ScoreTemp, [Rank, File, NewScore]):-
        Evaluation > AttackerEval, 
	NewScore is ScoreTemp + Evaluation, !.

check_for_threat(Rank, File, Evaluation, [DefenderEval|OtherDefenders], [AttackerEval|OtherAttackers], ScoreTemp, Threats):-
	NewScore is ScoreTemp + Evaluation - AttackerEval,
	check_for_threat(Rank, File, DefenderEval, OtherDefenders, OtherAttackers, NewScore, Threats).

target_piece(Colour, Rank, File, ChessBoard, DefendedTimes, PiecesDefending, AttackedTimes, PiecesAttacking):-
	kings_attacking_defending(Colour, Rank, File, ChessBoard, 0, 0, DefendedbyKing, AttackedbyKing),
	line_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimesLine, AttackedTimesLine, PiecesDefendingLine, PiecesAttackingLine),
	diagonal_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimesDiag, AttackedTimesDiag, PiecesDefendingDiag, PiecesAttackingDiag),
	knights_attacking_defending(Colour, Rank, File, ChessBoard, DefendedbyKnights, AttackedbyKnights),
	pawns_attacking_defending(Colour, Rank, File, ChessBoard, DefendedbyPawns, AttackedbyPawns),
	fix_kings_attacking_defending(Colour, DefendedbyKing, AttackedbyKing, PiecesDefendingKings, PiecesAttackingKings),
	fix_knights_attacking_defending(Colour, DefendedbyKnights, AttackedbyKnights, PiecesDefendingKnights, PiecesAttackingKnights),
	fix_pawns_attacking_defending(Colour, DefendedbyPawns, AttackedbyPawns, PiecesDefendingPawns, PiecesAttackingPawns),
	DefendedTimes is DefendedbyKing + DefendedbyKnights + DefendedbyPawns + DefendedTimesLine + DefendedTimesDiag,
	AttackedTimes is AttackedbyKing + AttackedbyKnights + AttackedbyPawns + AttackedTimesLine + AttackedTimesDiag,
	append([PiecesDefendingKings, PiecesDefendingKnights, PiecesDefendingPawns, PiecesDefendingLine, PiecesDefendingDiag], PiecesDefending),
	append([PiecesAttackingKings, PiecesAttackingKnights, PiecesAttackingPawns, PiecesAttackingLine, PiecesAttackingDiag], PiecesAttacking).

%%%%%%  FIX  %%%%%%

fix_kings_attacking_defending(_, 0, 0, [], []):- !.

fix_kings_attacking_defending(white, 1, 1, ['K'], ['k']):- !.
fix_kings_attacking_defending(white, 1, 0, ['K'], []):- !.
fix_kings_attacking_defending(white, 0, 1, [], ['k']).
fix_kings_attacking_defending(black, 1, 1, ['k'], ['K']):- !.
fix_kings_attacking_defending(black, 1, 0, ['k'], []):- !.
fix_kings_attacking_defending(black, 0, 1, [], ['K']).

fix_knights_attacking_defending(_, 0, 0, [], []):- !.

fix_knights_attacking_defending(white, 0, Att, DefPieces, ['n'|Tail]):-
	!,NewAtt is Att - 1,
	fix_knights_attacking_defending(white, 0, NewAtt, DefPieces, Tail).

fix_knights_attacking_defending(black, 0, Att, DefPieces, ['N'|Tail]):-
	!,NewAtt is Att - 1,
	fix_knights_attacking_defending(white, 0, NewAtt, DefPieces, Tail).

fix_knights_attacking_defending(white, Def, Att, ['N'|Tail], AttPieces):-
	NewDef is Def - 1,
	fix_knights_attacking_defending(white, NewDef, Att, Tail, AttPieces).

fix_knights_attacking_defending(black, Def, Att, ['n'|Tail], AttPieces):-
	NewDef is Def - 1,
	fix_knights_attacking_defending(white, NewDef, Att, Tail, AttPieces).

fix_pawns_attacking_defending(_, 0, 0, [], []):- !.

fix_pawns_attacking_defending(white, 0, Att, DefPieces, ['p'|Tail]):-
	!,NewAtt is Att - 1,
	fix_pawns_attacking_defending(white, 0, NewAtt, DefPieces, Tail).

fix_pawns_attacking_defending(black, 0, Att, DefPieces, ['P'|Tail]):-
	!,NewAtt is Att - 1,
	fix_pawns_attacking_defending(white, 0, NewAtt, DefPieces, Tail).

fix_pawns_attacking_defending(white, Def, Att, ['P'|Tail], AttPieces):-
	NewDef is Def - 1,
	fix_pawns_attacking_defending(white, NewDef, Att, Tail, AttPieces).

fix_pawns_attacking_defending(black, Def, Att, ['p'|Tail], AttPieces):-
	NewDef is Def - 1,
	fix_pawns_attacking_defending(white, NewDef, Att, Tail, AttPieces).

%%%%%%  PAWNS    %%%%%%

pawns_attacking_defending(white, 0, 0, ChessBoard, DefendedbyPawns, 0):-
	!, identify_piece([1,1], ChessBoard, Piece),
	count_pawns([Piece], 0, 0, DefendedbyPawns, _).

pawns_attacking_defending(white, 0, 7, ChessBoard, DefendedbyPawns, 0):-
	!, identify_piece([1,6], ChessBoard, Piece),
	count_pawns([Piece], 0, 0, DefendedbyPawns, _).

pawns_attacking_defending(white, 7, 0, ChessBoard, 0, AttackedbyPawns):-
	!, identify_piece([6,1], ChessBoard, Piece),
	count_pawns([Piece], 0, 0, _, AttackedbyPawns).

pawns_attacking_defending(white, 7, 7, ChessBoard, 0, AttackedbyPawns):-
	!, identify_piece([6,6], ChessBoard, Piece),
	count_pawns([Piece], 0, 0, _, AttackedbyPawns).

pawns_attacking_defending(black, 0, 0, ChessBoard, 0, AttackedbyPawns):-
        !, identify_piece([1,1], ChessBoard, Piece),
        count_pawns([Piece], 0, 0, AttackedbyPawns, _).

pawns_attacking_defending(black, 0, 7, ChessBoard, 0, AttackedbyPawns):-
        !, identify_piece([1,6], ChessBoard, Piece),
        count_pawns([Piece], 0, 0, AttackedbyPawns, _).

pawns_attacking_defending(black, 7, 0, ChessBoard, DefendedbyPawns, 0):-
        !, identify_piece([6,1], ChessBoard, Piece),
        count_pawns([Piece], 0, 0, _, DefendedbyPawns).

pawns_attacking_defending(black, 7, 7, ChessBoard, DefendedbyPawns, 0):-
        !, identify_piece([6,6], ChessBoard, Piece),
        count_pawns([Piece], 0, 0, _, DefendedbyPawns).

pawns_attacking_defending(white, 0, File, ChessBoard, DefendedbyPawns, 0):-
	!, NewFile1 is File - 1,
	NewFile2 is File + 1,
	identify_piece([1, NewFile1], ChessBoard, Piece1), 
	identify_piece([1, NewFile2], ChessBoard, Piece2),
	count_pawns([Piece1, Piece2], 0, 0, DefendedbyPawns, _).
	
pawns_attacking_defending(white, 7, File, ChessBoard, 0, AttackedbyPawns):-
	!, NewFile1 is File - 1,
	NewFile2 is File + 1,
	identify_piece([6, NewFile1], ChessBoard, Piece1),
	identify_piece([6, NewFile2], ChessBoard, Piece2),
	count_pawns([Piece1, Piece2], 0, 0, _, AttackedbyPawns).

pawns_attacking_defending(white, Rank, 0, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
	!, NewRank1 is Rank - 1,
	NewRank2 is Rank + 1,
	identify_piece([NewRank1, 1], ChessBoard, Piece1),
	count_pawns([Piece1], 0, 0, _, AttackedbyPawns),
	identify_piece([NewRank2, 1], ChessBoard, Piece2),
	count_pawns([Piece2], 0, 0, DefendedbyPawns, _).

pawns_attacking_defending(white, Rank, 7, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
	!, NewRank1 is Rank - 1,
	NewRank2 is Rank + 1,
	identify_piece([NewRank1, 6], ChessBoard, Piece1),
	count_pawns([Piece1], 0, 0, _, AttackedbyPawns),
	identify_piece([NewRank2, 6], ChessBoard, Piece2),
	count_pawns([Piece2], 0, 0, DefendedbyPawns, _).

pawns_attacking_defending(black, 0, File, ChessBoard, 0, AttackedbyPawns):-
        !, NewFile1 is File - 1,
        NewFile2 is File + 1,
        identify_piece([1, NewFile1], ChessBoard, Piece1),
        identify_piece([1, NewFile2], ChessBoard, Piece2),
        count_pawns([Piece1, Piece2], 0, 0, AttackedbyPawns, _).

pawns_attacking_defending(black, 7, File, ChessBoard, DefendedbyPawns, 0):-
        !, NewFile1 is File - 1,
        NewFile2 is File + 1,
        identify_piece([6, NewFile1], ChessBoard, Piece1),
        identify_piece([6, NewFile2], ChessBoard, Piece2),
        count_pawns([Piece1, Piece2], 0, 0, _, DefendedbyPawns).

pawns_attacking_defending(black, Rank, 0, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
        !, NewRank1 is Rank - 1,
        NewRank2 is Rank + 1,
        identify_piece([NewRank1, 1], ChessBoard, Piece1),
        count_pawns([Piece1], 0, 0, _, DefendedbyPawns),
        identify_piece([NewRank2, 1], ChessBoard, Piece2),
        count_pawns([Piece2], 0, 0, AttackedbyPawns, _).

pawns_attacking_defending(black, Rank, 7, ChessBoard, DefendedbyPawns, AttackedbyPawns):-
        !, NewRank1 is Rank - 1,
        NewRank2 is Rank + 1,
        identify_piece([NewRank1, 6], ChessBoard, Piece1),
        count_pawns([Piece1], 0, 0, _, DefendedbyPawns),
        identify_piece([NewRank2, 6], ChessBoard, Piece2),
        count_pawns([Piece2], 0, 0, AttackedbyPawns, _).

pawns_attacking_defending(white, Rank, File, ChessBoard, DefendedbyPawns,AttackedbyPawns):-
	RankMinus is Rank - 1, 
	RankPlus is Rank + 1,
	FileMinus is File - 1,
	FilePlus is File + 1,
	identify_piece([RankMinus, FileMinus], ChessBoard, Piece1), 	
	identify_piece([RankMinus, FilePlus], ChessBoard, Piece2), 	
	identify_piece([RankPlus, FileMinus], ChessBoard, Piece3), 	
	identify_piece([RankPlus, FilePlus], ChessBoard, Piece4), 
	count_pawns([Piece1, Piece2], 0, 0, _, AttackedbyPawns),
	count_pawns([Piece3, Piece4], 0, 0, DefendedbyPawns, _).

pawns_attacking_defending(black, Rank, File, ChessBoard, DefendedbyPawns,AttackedbyPawns):-
        RankMinus is Rank - 1,   
        RankPlus is Rank + 1,
        FileMinus is File - 1,
        FilePlus is File + 1,
        identify_piece([RankMinus, FileMinus], ChessBoard, Piece1),     
        identify_piece([RankMinus, FilePlus], ChessBoard, Piece2), 
        identify_piece([RankPlus, FileMinus], ChessBoard, Piece3), 
        identify_piece([RankPlus, FilePlus], ChessBoard, Piece4),  
        count_pawns([Piece1, Piece2], 0, 0, _, DefendedbyPawns),
        count_pawns([Piece3, Piece4], 0, 0, AttackedbyPawns, _).	

count_pawns([], Whites, Blacks, Whites, Blacks).

count_pawns(['p'|T], WhiteTemp, BlackTemp, WhitePawns, BlackPawns):- 
	!, NewBlack is BlackTemp + 1, 
	count_pawns(T, WhiteTemp, NewBlack, WhitePawns, BlackPawns).

count_pawns(['P'|T], WhiteTemp, BlackTemp, WhitePawns, BlackPawns):-
	!, NewWhite is WhiteTemp + 1,
	count_pawns(T, NewWhite, BlackTemp, WhitePawns, BlackPawns).

count_pawns([_|T], WhiteTemp, BlackTemp, WhitePawns, BlackPawns):-
	count_pawns(T, WhiteTemp, BlackTemp, WhitePawns, BlackPawns).
	
	
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
	DefendedbyKnights is DefOneOClock + DefTwoOClock + DefFourOClock + DefFiveOClock + DefSevenOClock + DefEightOClock + DefTenOClock + DefElevenOClock,
	AttackedbyKnights is AttOneOClock + AttTwoOClock + AttFourOClock + AttFiveOClock + AttSevenOClock + AttEightOClock + AttTenOClock + AttElevenOClock.
	
elevenOClock_knight(_, 0, _, _, 0, 0):- !.
elevenOClock_knight(_, 1, _, _, 0, 0):- !.
elevenOClock_knight(_, _, 0, _, 0, 0):- !.

elevenOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
        TargetRank is Rank - 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

elevenOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank - 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

elevenOClock_knight(_, _, _, _, 0, 0).

tenOClock_knight(_, 0, _, _, 0, 0):- !.
tenOClock_knight(_, _, 0, _, 0, 0):- !.
tenOClock_knight(_, _, 1, _, 0, 0):- !.

tenOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
        TargetRank is Rank - 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

tenOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank - 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

tenOClock_knight(_, _, _, _, 0, 0).

eightOClock_knight(_, 7, _, _, 0, 0):- !.
eightOClock_knight(_, _, 0, _, 0, 0):- !.
eightOClock_knight(_, _, 1, _, 0, 0):- !.

eightOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
        TargetRank is Rank + 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

eightOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank + 1,
        TargetFile is File - 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

eightOClock_knight(_, _, _, _, 0, 0).

sevenOClock_knight(_, 6, _, _, 0, 0):- !.
sevenOClock_knight(_, 7, _, _, 0, 0):- !.
sevenOClock_knight(_, _, 0, _, 0, 0):- !.

sevenOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
        TargetRank is Rank + 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

sevenOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank + 2,
        TargetFile is File - 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

sevenOClock_knight(_, _, _, _, 0, 0).

fiveOClock_knight(_, 6, _, _, 0, 0):- !.
fiveOClock_knight(_, 7, _, _, 0, 0):- !.
fiveOClock_knight(_, _, 7, _, 0, 0):- !.

fiveOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
        TargetRank is Rank + 2,
        TargetFile is File + 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

fiveOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank + 2,
        TargetFile is File + 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

fiveOClock_knight(_, _, _, _, 0, 0).

fourOClock_knight(_, 7, _, _, 0, 0):- !.
fourOClock_knight(_, _, 6, _, 0, 0):- !.
fourOClock_knight(_, _, 7, _, 0, 0):- !.

fourOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
        TargetRank is Rank + 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

fourOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank + 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(AttColour, Colour), !.

fourOClock_knight(_, _, _, _, 0, 0).

twoOClock_knight(_, 0, _, _, 0, 0):- !.
twoOClock_knight(_, _, 6, _, 0, 0):- !.
twoOClock_knight(_, _, 7, _, 0, 0):- !.

twoOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
        TargetRank is Rank - 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
        knight_piece(DefPiece),
        colour(DefPiece, Colour), !.

twoOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank - 1,
        TargetFile is File + 2,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour), 
        reverse_colour(AttColour, Colour), !.

twoOClock_knight(_, _, _, _, 0, 0).

oneOClock_knight(_, 0, _, _, 0, 0):- !.
oneOClock_knight(_, 1, _, _, 0, 0):- !.
oneOClock_knight(_, _, 7, _, 0, 0):- !.

oneOClock_knight(Colour, Rank, File, ChessBoard, 1, 0):-
	TargetRank is Rank - 2,
	TargetFile is File + 1,
	identify_piece([TargetRank, TargetFile], ChessBoard, DefPiece),
	knight_piece(DefPiece),
	colour(DefPiece, Colour), !.

oneOClock_knight(Colour, Rank, File, ChessBoard, 0, 1):-
        TargetRank is Rank - 2,
        TargetFile is File + 1,
        identify_piece([TargetRank, TargetFile], ChessBoard, AttPiece),
        knight_piece(AttPiece),
        colour(AttPiece, AttColour), 
	reverse_colour(AttColour, Colour), !.

oneOClock_knight(_, _, _, _, 0, 0).
	

%%%%%%  DIAGONAL PIECES  %%%%%%%

diagonal_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
	upleft_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, UpleftDefendedTimes, UpleftAttackedTimes, UpleftPiecesDefending, UpleftPiecesAttacking),
	upright_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, UprightDefendedTimes, UprightAttackedTimes, UprightPiecesDefending, UprightPiecesAttacking),
	downleft_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, DownleftDefendedTimes, DownleftAttackedTimes, DownleftPiecesDefending, DownleftPiecesAttacking),
	downright_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, DownrightDefendedTimes, DownrightAttackedTimes, DownrightPiecesDefending, DownrightPiecesAttacking),
	DefendedTimes is UpleftDefendedTimes + UprightDefendedTimes + DownleftDefendedTimes + DownrightDefendedTimes,
	AttackedTimes is UpleftAttackedTimes + UprightAttackedTimes + DownleftAttackedTimes + DownrightAttackedTimes, 
	append([UpleftPiecesDefending, UprightPiecesDefending, DownleftPiecesDefending, DownrightPiecesDefending], PiecesDefending),
	append([UpleftPiecesAttacking, UprightPiecesAttacking, DownleftPiecesAttacking, DownrightPiecesAttacking], PiecesAttacking).	

downright_attackers_defenders(_, 7, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.
downright_attackers_defenders(_, _, 7, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

downright_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        downright_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

downright_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

downright_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

downright_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []).

downleft_attackers_defenders(_, 7, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.
downleft_attackers_defenders(_, _, 0, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

downleft_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        downleft_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

downleft_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

downleft_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

downleft_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []).

upright_attackers_defenders(_, 0, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.
upright_attackers_defenders(_, _, 7, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

upright_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        upright_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

upright_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

upright_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

upright_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []).


upleft_attackers_defenders(_, 0, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.
upleft_attackers_defenders(_, _, 0, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

upleft_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
	NewFile is File - 1, 
	NewRank is Rank - 1,
	identify_piece([NewRank, NewFile], ChessBoard, ' '), !, 
	upleft_attackers_defenders(Colour, NewRank, NewFile, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

upleft_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
	NewFile is File - 1,
	NewRank is Rank - 1,
	identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
	diagonal_piece(AttPiece),
	colour(AttPiece, AttColour),
	reverse_colour(Colour, AttColour), !,
	NewAtt is AttTemp + 1, 
	find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

upleft_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
	NewFile is File - 1,
	NewRank is Rank - 1,
	identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
	diagonal_piece(DefPiece), !,
	NewDef is DefTemp + 1,
	find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

upleft_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []).

%%%%%%  LINE PIECES  %%%%%%%   %%% FIX BATTERY MAYBE?? -- SAME AS DIAG %%%

line_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):- 
	north_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, NorthDefendedTimes, NorthAttackedTimes, NorthPiecesDefending, NorthPiecesAttacking),
	south_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, SouthDefendedTimes, SouthAttackedTimes, SouthPiecesDefending, SouthPiecesAttacking),
	west_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, WestDefendedTimes, WestAttackedTimes, WestPiecesDefending, WestPiecesAttacking),
	east_attackers_defenders(Colour, Rank, File, ChessBoard, 0, 0, EastDefendedTimes, EastAttackedTimes, EastPiecesDefending, EastPiecesAttacking),
	DefendedTimes is NorthDefendedTimes + SouthDefendedTimes + WestDefendedTimes + EastDefendedTimes,
	AttackedTimes is NorthAttackedTimes + SouthAttackedTimes + WestAttackedTimes + EastAttackedTimes, 
	append([NorthPiecesDefending, SouthPiecesDefending, WestPiecesDefending, EastPiecesDefending], PiecesDefending),
	append([NorthPiecesAttacking, SouthPiecesAttacking, WestPiecesAttacking, EastPiecesAttacking], PiecesAttacking).
	
east_attackers_defenders(_, _, 7, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

east_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        east_attackers_defenders(Colour, Rank, NewFile, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

east_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
      	NewFile is File + 1,
	identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

east_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
       	NewFile is File + 1,
	identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece), !,
        NewDef is DefTemp + 1,
	find_battery(Colour, east, Rank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

east_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []).

west_attackers_defenders(_, _, 0, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

west_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
        NewFile is File - 1,
	identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        west_attackers_defenders(Colour, Rank, NewFile, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

west_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
        NewFile is File - 1,
	identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
	find_battery(Colour, west, Rank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

west_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece), !,
        NewDef is DefTemp + 1,
	find_battery(Colour, west, Rank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).
	
west_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []).

south_attackers_defenders(_, 7, _, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

south_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
	NewRank is Rank + 1,
	identify_piece([NewRank, File], ChessBoard, ' '), !, 
	south_attackers_defenders(Colour, NewRank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

south_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
	NewRank is Rank + 1,
	identify_piece([NewRank, File], ChessBoard, AttPiece), 
	line_piece(AttPiece),
	colour(AttPiece, AttColour),
	reverse_colour(Colour, AttColour), !,
	NewAtt is AttTemp + 1,
	find_battery(Colour, south, NewRank, File, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

south_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
	NewRank is Rank + 1,
	identify_piece([NewRank, File], ChessBoard, DefPiece),
	line_piece(DefPiece), !,
	NewDef is DefTemp + 1,
	find_battery(Colour, south, NewRank, File, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

south_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []). 
		
north_attackers_defenders(_, 0, _, _, DefTimes, AttTimes, DefTimes, AttTimes, [], []):- !.

north_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, ' '), !,
        north_attackers_defenders(Colour, NewRank, File, ChessBoard, DefTemp, AttTemp, DefendedTimes, AttackedTimes, PiecesDefending, PiecesAttacking).

north_attackers_defenders(Colour, Rank, File, ChessBoard, DefendedTimes, AttTemp, DefendedTimes, AttackedTimes, [], [AttPiece|Tail]):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
	find_battery(Colour, north, NewRank, File, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).
	
north_attackers_defenders(Colour, Rank, File, ChessBoard, DefTemp, AttackedTimes, DefendedTimes, AttackedTimes, [DefPiece|Tail], []):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, DefPiece),
        line_piece(DefPiece), !,
        NewDef is DefTemp + 1,
	find_battery(Colour, north, NewRank, File, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

north_attackers_defenders(_, _, _, _, DefendedTimes, AttackedTimes, DefendedTimes, AttackedTimes, [], []).    

find_battery(_, east, _, 7, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, west, _, 0, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, north, 0, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, south, 7, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.

find_battery(_, upleft, 0, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, upleft, _, 0, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, upright, 0, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, upright, _, 7, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, downleft, 7, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, downleft, _, 0, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, downright, 7, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.
find_battery(_, downright, _, 7, _, _, MakeTheseTheSame, MakeTheseTheSame, []):- !.

find_battery(Colour, downright, Rank, File, ChessBoard, AttackorDefence, Temp, Times, Pieces):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, AttackorDefence, Temp, Times, Pieces).

find_battery(Colour, downright, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece|Tail]):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, downright, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece|Tail]):-
        NewFile is File + 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, downright, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, downright, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).

find_battery(Colour, downleft, Rank, File, ChessBoard, AttackorDefence, Temp, Times, Pieces):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, AttackorDefence, Temp, Times, Pieces).

find_battery(Colour, downleft, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece|Tail]):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, downleft, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece|Tail]):-
        NewFile is File - 1,
        NewRank is Rank + 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, downleft, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, downleft, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).

find_battery(Colour, upleft, Rank, File, ChessBoard, AttackorDefence, Temp, Times, Pieces):-
        NewFile is File - 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, AttackorDefence, Temp, Times, Pieces).

find_battery(Colour, upleft, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece|Tail]):-
        NewFile is File - 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, upleft, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece|Tail]):-
        NewFile is File - 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, upleft, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, upleft, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).

find_battery(Colour, upright, Rank, File, ChessBoard, AttackorDefence, Temp, Times, Pieces):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, AttackorDefence, Temp, Times, Pieces).

find_battery(Colour, upright, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece|Tail]):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, AttPiece),
        diagonal_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, upright, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece|Tail]):-
        NewFile is File + 1,
        NewRank is Rank - 1,
        identify_piece([NewRank, NewFile], ChessBoard, DefPiece),
        diagonal_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, upright, NewRank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, upright, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).

find_battery(Colour, east, Rank, File, ChessBoard, AttackOrDefence, Temp, Times, Pieces):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, AttackOrDefence, Temp, Times, Pieces).

find_battery(Colour, east, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece | Tail]):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, east, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece | Tail]):-
        NewFile is File + 1,
        identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, east, Rank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, east, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).

find_battery(Colour, west, Rank, File, ChessBoard, AttackOrDefence, Temp, Times, Pieces):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, ' '), !,
        find_battery(Colour, west, Rank, NewFile, ChessBoard, AttackOrDefence, Temp, Times, Pieces).

find_battery(Colour, west, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece | Tail]):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, west, Rank, NewFile, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, west, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece | Tail]):-
        NewFile is File - 1,
        identify_piece([Rank, NewFile], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, west, Rank, NewFile, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, west, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).

find_battery(Colour, north, Rank, File, ChessBoard, AttackOrDefence, Temp, Times, Pieces):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, ' '), !,
        find_battery(Colour, north, NewRank, File, ChessBoard, AttackOrDefence, Temp, Times, Pieces).

find_battery(Colour, north, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece | Tail]):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, north, NewRank, File, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, north, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece | Tail]):-
        NewRank is Rank - 1,
        identify_piece([NewRank, File], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, north, NewRank, File, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, north, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).

find_battery(Colour, south, Rank, File, ChessBoard, AttackOrDefence, Temp, Times, Pieces):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], ChessBoard, ' '), !,
        find_battery(Colour, south, NewRank, File, ChessBoard, AttackOrDefence, Temp, Times, Pieces).

find_battery(Colour, south, Rank, File, ChessBoard, attacking_battery, AttTemp, AttackedTimes, [AttPiece | Tail]):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], ChessBoard, AttPiece),
        line_piece(AttPiece),
        colour(AttPiece, AttColour),
        reverse_colour(Colour, AttColour), !,
        NewAtt is AttTemp + 1,
        find_battery(Colour, south, NewRank, File, ChessBoard, attacking_battery, NewAtt, AttackedTimes, Tail).

find_battery(Colour, south, Rank, File, ChessBoard, defending_battery, DefTemp, DefendedTimes, [DefPiece | Tail]):-
        NewRank is Rank + 1,
        identify_piece([NewRank, File], ChessBoard, DefPiece),
        line_piece(DefPiece),
        colour(DefPiece, Colour), !,
        NewDef is DefTemp + 1,
        find_battery(Colour, south, NewRank, File, ChessBoard, defending_battery, NewDef, DefendedTimes, Tail).

find_battery(_, south, _, _, _, _, MakeTheseTheSame, MakeTheseTheSame, []).




%%%%%%  KINGS  %%%%%%%

kings_attacking_defending(Colour, 0, 0, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
	find_kings_on_squares(Colour, [[0,1],[1,0],[1,1]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing). 
	
kings_attacking_defending(Colour, 0, 7, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
	find_kings_on_squares(Colour, [[0,6],[1,6],[1,7]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 7, 0, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
        find_kings_on_squares(Colour, [[6,0],[6,1],[7,1]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 7, 7, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
        find_kings_on_squares(Colour, [[7,6],[6,7],[6,6]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 0, File, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
	FileMinus is File - 1, 
	FilePlus is File + 1,
	find_kings_on_squares(Colour, [[0,FileMinus],[0,FilePlus],[1,FileMinus],[1,File],[1,FilePlus]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, 7, File, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
        FileMinus is File - 1,
        FilePlus is File + 1,
        find_kings_on_squares(Colour, [[7,FileMinus],[7,FilePlus],[6,FileMinus],[6,File],[6,FilePlus]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, Rank, 0, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
        RankMinus is Rank - 1,
        RankPlus is Rank + 1, 
        find_kings_on_squares(Colour, [[RankMinus,0],[RankPlus,0],[RankMinus,1],[Rank,1],[RankPlus,1]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, Rank, 7, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):- !,
        RankMinus is Rank - 1, 
        RankPlus is Rank + 1,
        find_kings_on_squares(Colour, [[RankMinus,7],[RankPlus,7],[RankMinus,6],[Rank,6],[RankPlus,6]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

kings_attacking_defending(Colour, Rank, File, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):-
	RankMinus is Rank - 1,
        RankPlus is Rank + 1,
	FileMinus is File - 1,
        FilePlus is File + 1,
	find_kings_on_squares(Colour, [[RankMinus,FileMinus], [RankMinus, File], [RankMinus, FilePlus], [Rank, FileMinus], [Rank, FilePlus], [RankPlus, FileMinus], [RankPlus, File], [RankPlus, FilePlus]], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).
	
find_kings_on_squares(_, [], _, Def, Att, Def, Att):- !.

find_kings_on_squares(white, [H|T], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):-
	identify_piece(H, ChessBoard, 'K'), !, 
	NewDef is DefTemp + 1,
	find_kings_on_squares(white, T, ChessBoard, NewDef, AttTemp, DefendedbyKing, AttackedbyKing). 

find_kings_on_squares(white, [H|T], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):-
	identify_piece(H, ChessBoard, 'k'), !,
	NewAtt is AttTemp + 1, 
	find_kings_on_squares(white, T, ChessBoard, DefTemp, NewAtt, DefendedbyKing, AttackedbyKing).

find_kings_on_squares(white, [_|T], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):-
	find_kings_on_squares(white, T, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

find_kings_on_squares(black, [H|T], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):-
	identify_piece(H, ChessBoard, 'k'), !,
	NewDef is DefTemp + 1,
	find_kings_on_squares(black, T, ChessBoard, NewDef, AttTemp, DefendedbyKing, AttackedbyKing).

find_kings_on_squares(black, [H|T], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):-
	identify_piece(H, ChessBoard, 'K'), !,
	NewAtt is AttTemp + 1,
	find_kings_on_squares(black, T, ChessBoard, DefTemp, NewAtt, DefendedbyKing, AttackedbyKing).

find_kings_on_squares(black, [_|T], ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing):-
        find_kings_on_squares(black, T, ChessBoard, DefTemp, AttTemp, DefendedbyKing, AttackedbyKing).

