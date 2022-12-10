%%%%%%%%%%%%%%%%%%%%     FEN -> STATE      %%%%%%%%%%%%%%%%%%%%

initialize_state(InputFEN, ChessBoard, Next_to_Play, Castling, En_Passant, Half_Moves, Moves_Count):- 
	fen_parser(InputFEN, InputList),
	concat_ranks(InputList,8,[],ChessBoard),  %8 iterations to take each rank form inp to state
	nth0(8, InputList, Next_to_Play),
	nth0(9, InputList, Castling),
	nth0(10, InputList, En_Passant), 
	nth0(11, InputList, Half_Moves),
	nth0(12, InputList, Moves_Count).

%% FEN strings contain the following elements:
%	- Seven "/" separating the eight ranks of the chess board.
%	- At most eight characters for each rank, specifying the piece occupying the square of the corresponding column. Empty squares are specified with a number [1-8], denoting the number of consecutive empty squares.
%   - The characters corresponding to each piece: 'k'ing, 'q'ueen, 'b'ishop, k'n'ight, 'r'ook and 'p'awn. Capital letters denote white pieces and lowercase black pieces. 
%   - There is a space after the specification of the final rank. It is followed with a space seperated list with character sequences denoting: (i) player to move, (ii) castling rights, (iii) en-passant square, (iv) halfmove clock, (v) fullmove clock.

% Example: rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 
% This is the starting position after white has played e4. 
% It is translated on the chess board from top to bottom, i.e., rnbqkbnr specifies the position of all black pieces, except pawns, on the eighth (top) rank.
% The sixth, fifth and third ranks are empty, i.e., contain eight empty squares.
% The fourth rank contains only one pawn on the 'e' file.
% The corresponding square in the second rank is empty.
% The 'b' character after the specification of the chess board signifies that it is black's turn to move.
% KQkq denotes that both player maintain the castling rights for both the king's side and the queen's side. 'K' and 'Q' designate king's and queen's side castling rights for white, and 'k' and 'q' the respective castling right's for black.
% The halfmove clock is currently zero, because the last move was a pawn move.
% The fullmove clock is one, because we are on the first move. Both players have to move for this counter to advance.

% fen_parser(+Input, -OutState)
% The input FEN string, given as an atom, is translated to a list containing the ranks of the chessboard and the suffix FEN information.
fen_parser(FENAtom, RanksAndInfo):-
		atom_chars(FENAtom, FENChars),
		seperate_at_char(FENChars, '/', RanksAndInfo),

% seperate_at_char(+Chars, +SepChar, -ListsOfChars)
% Copies list Chars to list of lists ListsOfChars.
% Whenever it finds SepChar in Chars, it does not copy the character.
% Instead, it moves to the next list in ListsOfChars.
% WARNING: Numberic atoms are also transformed into numbers.
seperate_at_char([], _SepChar, [[]|[]]).
seperate_at_char([SepChar|RestChars], SepChar, [[] | RestLists]):- 
	!,
	seperate_at_char(RestChars, SepChar, RestLists).
seperate_at_char([Char|RestChars], SepChar, [[Char | RestInList] |RestLists]):- 
	seperate_at_char(RestChars, SepChar, [RestInList|RestLists]).

%concat_ranks([], []).
%concat_ranks([RankOrInfo|RestRankAndInfo], []):-
	%parse_rank(RankOrInfo, 

%parse_rank([], )
%parse_rank([Char|RestChar], ):-
	%number_chars(Number, [Char]), !, 
	%NumberMinusOne is Number - 1, 
	%parse_rank([NumberMinusOne|Rest

% 
concat_ranks(_,0,C,C):- !.
concat_ranks([H|T],Iter,Prev,ChessBoard):-
		string_chars(H, Pieces),
		fix_spaces(Pieces,[], FixedPieces),
		append(Prev, [FixedPieces], NewPrev),
		NewIter is Iter-1,
        concat_ranks(T,NewIter,NewPrev,ChessBoard).

fix_spaces([],Same,Same):-
        !.

fix_spaces([H|T],Dummy,FixedPieces):-
		char_type(H, digit),!,
		atom_number(H,NumofSpaces),
		insert_spaces(NumofSpaces,Dummy,New),
		fix_spaces(T,New,FixedPieces).

fix_spaces([H|T],Dummy,FixedPieces):-
		char_type(H,alpha),
		append(Dummy,[H],New),
		fix_spaces(T,New,FixedPieces).

insert_spaces(0, Rest, Rest):-
        !.
insert_spaces(Iter,Dummy,Result):-
		append(Dummy,[' '],NewDummy),
		NewIter is Iter - 1,
		insert_spaces(NewIter,NewDummy, Result).

