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
        append([Piece], H,  AttackedPiece),
        attacked_pieces(Colour, State, T, Tail).

%%%%%%%%%%%%%%%%%%%%%%%     MATERIAL EVAL     %%%%%%%%%%%%%%%%%%%%%%%%%

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

fix_indexes(ParsedMove, [OldRank, OldFile, NewRank, NewFile, PromotedTo]):-
	length(ParsedMove, 5), !, 
	nth0(0, ParsedMove, FileLetter),
        file_mapping(FileLetter, OldFile),
        nth0(2, ParsedMove, FileLetter1),
        file_mapping(FileLetter1, NewFile),
        nth0(1, ParsedMove, NotationRank),
        fix_rank_index(NotationRank, OldRank),
        nth0(3, ParsedMove, NotationRank1),
        fix_rank_index(NotationRank1, NewRank),
	nth0(4, ParsedMove, PromotedTo).

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

index_to_square([0,0], "a8"):- !.
index_to_square([0,1],"b8"):- !.
index_to_square([0,2],"c8"):- !.
index_to_square([0,3],"d8"):- !.
index_to_square([0,4],"e8"):- !.
index_to_square([0,5],"f8"):- !.
index_to_square([0,6],"g8"):- !.
index_to_square([0,7],"h8"):- !.
index_to_square([1,0],"a7"):- !.
index_to_square([1,1],"b7"):- !.
index_to_square([1,2],"c7"):- !.
index_to_square([1,3],"d7"):- !.
index_to_square([1,4],"e7"):- !.
index_to_square([1,5],"f7"):- !.
index_to_square([1,6],"g7"):- !.
index_to_square([1,7],"h7"):- !.
index_to_square([2,0],"a6"):- !.
index_to_square([2,1],"b6"):- !.
index_to_square([2,2],"c6"):- !.
index_to_square([2,3],"d6"):- !.
index_to_square([2,4],"e6"):- !.
index_to_square([2,5],"f6"):- !.
index_to_square([2,6],"g6"):- !.
index_to_square([2,7],"h6"):- !.
index_to_square([3,0],"a5"):- !.
index_to_square([3,1],"b5"):- !.
index_to_square([3,2],"c5"):- !.
index_to_square([3,3],"d5"):- !.
index_to_square([3,4],"e5"):- !.
index_to_square([3,5],"f5"):- !.
index_to_square([3,6],"g5"):- !.
index_to_square([3,7],"h5"):- !.
index_to_square([4,0],"a4"):- !.
index_to_square([4,1],"b4"):- !.
index_to_square([4,2],"c4"):- !.
index_to_square([4,3],"d4"):- !.
index_to_square([4,4],"e4"):- !.
index_to_square([4,5],"f4"):- !.
index_to_square([4,6],"g4"):- !.
index_to_square([4,7],"h4"):- !.
index_to_square([5,0],"a3"):- !.
index_to_square([5,1],"b3"):- !.
index_to_square([5,2],"c3"):- !.
index_to_square([5,3],"d3"):- !.
index_to_square([5,4],"e3"):- !.
index_to_square([5,5],"f3"):- !.
index_to_square([5,6],"g3"):- !.
index_to_square([5,7],"h3"):- !.
index_to_square([6,0],"a2"):- !.
index_to_square([6,1],"b2"):- !.
index_to_square([6,2],"c2"):- !.
index_to_square([6,3],"d2"):- !.
index_to_square([6,4],"e2"):- !.
index_to_square([6,5],"f2"):- !.
index_to_square([6,6],"g2"):- !.
index_to_square([6,7],"h2"):- !.
index_to_square([7,0],"a1"):- !.
index_to_square([7,1],"b1"):- !.
index_to_square([7,2],"c1"):- !.
index_to_square([7,3],"d1"):- !.
index_to_square([7,4],"e1"):- !.
index_to_square([7,5],"f1"):- !.
index_to_square([7,6],"g1"):- !.
index_to_square([7,7],"h1").

%%%%%%%%%%%%%%%%%%%%    VALID RANKS_FILES   %%%%%%%%%%%%%%%%%

file_letter('a').
file_letter('b').
file_letter('c').
file_letter('d').
file_letter('e').
file_letter('f').
file_letter('g').
file_letter('h').

rank_digit('1').
rank_digit('2').
rank_digit('3').
rank_digit('4').
rank_digit('5').
rank_digit('6').
rank_digit('7').
rank_digit('8').

%%%%%%%%%%%%%%%%%%%%     EVALS     %%%%%%%%%%%%%%%%%%%%%%%%%%

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
evaluate(' ', 0).

pieces_to_evaluations([],[]).

pieces_to_evaluations([H|T],[Head|Tail]):-
	evaluate(H, Head),
	pieces_to_evaluations(T,Tail).

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

%%%%% PLAYER %%%%%

player_to_colour("w", white).
player_to_colour("b", black).
%%%%%%%%%%%%%%%%%%%  PIECE PATTERNS  %%%%%%%%%%%%%%%%%%%%%

line_piece('R').
line_piece('r').
line_piece('Q').
line_piece('q').

diagonal_piece('B').
diagonal_piece('b').
diagonal_piece('Q').
diagonal_piece('q').

knight_piece('N').
knight_piece('n').

%%%%%%%%%%%%%%  DIAGONALS AND LINES  %%%%%%%%%%%%%%%%%%%%%%%

same_diagonal_as([Rank, File], AllSameDiagonal):-
	upleft_same_diagonal_as([Rank, File], Upleft),
	upright_same_diagonal_as([Rank, File], Upright),
	downleft_same_diagonal_as([Rank, File], Downleft),
	downright_same_diagonal_as([Rank, File], Downright),
	append([Upleft, Upright, Downleft, Downright], AllSameDiagonal).

upleft_same_diagonal_as([0, _], []):- !.

upleft_same_diagonal_as([_, 0], []):- !.

upleft_same_diagonal_as([Rank, File], [[RankNeg, FileNeg]|T]):-
	RankNeg is Rank - 1,
	FileNeg is File - 1,
	upleft_same_diagonal_as([RankNeg, FileNeg], T).

upright_same_diagonal_as([0, _], []):- !.

upright_same_diagonal_as([_, 7], []):- !.

upright_same_diagonal_as([Rank, File], [[RankNeg, FilePos]|T]):-
	RankNeg is Rank - 1, 
	FilePos is File + 1,
	upright_same_diagonal_as([RankNeg, FilePos],T).

downleft_same_diagonal_as([7, _], []):- !.

downleft_same_diagonal_as([_, 0], []):- !.

downleft_same_diagonal_as([Rank, File], [[RankPos, FileNeg]|T]):-
	RankPos is Rank + 1, 
	FileNeg is File - 1,
	downleft_same_diagonal_as([RankPos, FileNeg],T).

downright_same_diagonal_as([7, _], []):- !.

downright_same_diagonal_as([_, 7], []):- !.

downright_same_diagonal_as([Rank, File], [[RankPos, FilePos]|T]):-
	RankPos is Rank + 1, 
	FilePos is File + 1,
	upright_same_diagonal_as([RankPos, FilePos],T).

same_line_as([Rank, File], AllSameLine):-
	north_same_line_as([Rank, File], North),
	south_same_line_as([Rank, File], South),
	west_same_line_as([Rank, File], West),
	east_same_line_as([Rank, File], East),
	append([North, South, West, East], AllSameLine).

north_same_line_as([0, _], []):- !.

north_same_line_as([Rank, File], [[RankNeg, File]|T]):-
	RankNeg is Rank - 1,
 	north_same_line_as([RankNeg, File], T).

south_same_line_as([7, _], []):- !.

south_same_line_as([Rank, File], [[RankPos, File]|T]):-
	RankPos is Rank + 1,
 	south_same_line_as([RankPos, File], T).

west_same_line_as([_, 0], []):- !.

west_same_line_as([Rank, File], [[Rank, FileNeg]|T]):-
	FileNeg is File - 1, 
 	west_same_line_as([Rank, FileNeg], T).

east_same_line_as([_, 7], []):- !.

east_same_line_as([Rank, File], [[Rank, FilePos]|T]):-
	FilePos is File + 1,
 	east_same_line_as([Rank, FilePos], T).
