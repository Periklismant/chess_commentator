%%%%%%%%%%%%%%%%%%%  PIECE PATTERNS  %%%%%%%%%%%%%%%%%%%%%

queen('Q').
queen('q').

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

%%%%% PLAYER %%%%%

player_to_colour("w", white).
player_to_colour("b", black).

%%%%%%%%%%%%%%%%%%%%     COLOUR     %%%%%%%%%%%%%%%%%%%%%%%%%

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

reverse_colour(white, black).
reverse_colour(black, white).

%%%%% SQUARES %%%%%
colour(Rank, File, light):-
        S is Rank + File,
        K is S mod 2,
        K is 0, !.

colour(_, _, dark).

% file_id_number(?FileID,?FileNumber)
% fileID to number
file_id_number('a',1).
file_id_number('b',2).
file_id_number('c',3).
file_id_number('d',4).
file_id_number('e',5).
file_id_number('f',6).
file_id_number('g',7).
file_id_number('h',8).

%%%%%%%%%%%%%%%%%%% Fetch and Update Board %%%%%%%%%%%%%%%%

%get_rank(+RankNumber, +ChessBoard, -Rank)
% fetch rank <RankNumber> from the chessboard
% Count ranks from 1 to 8
get_rank(1, [NextRank|_RestRanks], NextRank):- !.
get_rank(N, [_NextRank|RestRanks], RequestedRank):-
	NMinusOne is N - 1,
	get_rank(NMinusOne, RestRanks, RequestedRank).

% get_nth_in_rank(+SquareNumber, +Rank, -Square)
% fetch the <SquareNumber>th square from the given rank.
% square a's number is 1, b's is 2, etc
get_nth_in_rank(1, [NextSquare|_RestSquares], NextSquare):- !.
get_nth_in_rank(N, [_NextSquare|RestSquares], RequestedSquare):-
	NMinusOne is N - 1,
	get_rank(NMinusOne, RestSquares, RequestedSquare).

% get_file(+FileID, +ChessBoard, -File)
% fetch the selected file from the chessboard
% file a's number is 1, b's is 2, etc
get_file(FileID, ChessBoard, File):-
	file_id_number(FileID, FileNum),
	get_file_num(FileNum, ChessBoard, File).
get_file_num(_N, _Ranks, []).
get_file_num(N, [NextRank|RestRanks], [NextSquare|RestSquaresInColumn]):-
	get_nth_in_rank(N, NextRank, NextSquare), 
	NMinusOne is N - 1,
	get_file_num(NMinusOne, RestRanks, RestSquaresInColumn).
		
