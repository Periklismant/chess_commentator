%%%%%%%%%%%%%%%     INCLUDES      %%%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].

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

