%%%%%%%%%%%%%%     INCLUDES     %%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].


%%%%%%%%%%%%%%      READ LINE FROM STOCKFISH     %%%%%%%%%%%%%%

get_best_line(File, Best_Line):-
	open(File, read, Stream),
	find_best_line(Stream, "", Best_Text_Line),
	split_string(Best_Text_Line, " ", "", Best_Text_List),
	get_moves_from_stockfish_text(Best_Text_List, Best_Line),
	close(Stream).

get_moves_from_stockfish_text([],[]).

get_moves_from_stockfish_text([FirstMove|RestMoves], [FirstMove|RestMoves]):-
	string_chars(FirstMove, [ValidLetter1, ValidDigit1, ValidLetter2, ValidDigit2 | _]), 
	file_letter(ValidLetter1),
	rank_digit(ValidDigit1),
	file_letter(ValidLetter2),
	rank_digit(ValidDigit2), !.

get_moves_from_stockfish_text([_Junk|T], Best_Line):-
	get_moves_from_stockfish_text(T, Best_Line). 	

find_best_line(Stream, Best_Text_Line, Best_Text_Line):-
	at_end_of_stream(Stream), !. 

find_best_line(Stream, _, Best_Text_Line):-
	readLine(Stream, CurrLine),
	split_string(CurrLine, " ", "", CurrList),
	nth0(0, CurrList, "info"), !, 
	find_best_line(Stream, CurrLine, Best_Text_Line).

find_best_line(Stream, PossibleBest, Best_Text_Line):-
	readLine(Stream, _),
	find_best_line(Stream, PossibleBest, Best_Text_Line).
	
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

readWord(InStream,W):-
	get_code(InStream,Char),
        checkCharAndReadRestWord(Char,Chars,InStream),
        atom_codes(W,Chars).
   
checkCharAndReadRestWord(10,[],_):-  !.
   
checkCharAndReadRestWord(32,[],_):-  !.
   
checkCharAndReadRestWord(-1,[],_):-  !.
   
checkCharAndReadRestWord(end_of_file,[],_):-  !.
   
checkCharAndReadRestWord(Char,[Char|Chars],InStream):-
	get_code(InStream,NextChar),
	checkCharAndReadRest(NextChar,Chars,InStream).                                        
