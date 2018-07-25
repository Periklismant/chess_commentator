%%%%%%%%%%%%%%%%%%%%     FEN -> STATE      %%%%%%%%%%%%%%%%%%%%

initialize_state(InputFEN, ChessBoard):- %FIX
        fen_parser(InputFEN, InputList),
        concat_ranks(InputList,8,[],ChessBoard).  %8 iterations to take each rank form inp to state

fen_parser(Input, OutState):-
        split_string(Input, "/", "", RanksList), %RankLists has 8 elems, 8th  elem contains otherargs(wb, en passant,...)
        nth0(7, RanksList, Rank8),
        split_string(Rank8," ", "", OtherArgs),  %Parse 8th elem. OtherArg has 6 elems
        list_butlast(RanksList, Ranks),
        append([Ranks,OtherArgs], OutState).

concat_ranks(_,0,C,C):-
        !.

concat_ranks([H|T],Iter,Prev,ChessBoard):-
        string_chars(H, Pieces),
        fix_spaces(Pieces,[], FixedPieces),
        append(Prev, [FixedPieces], NewPrev),
        NewIter is Iter-1,
        concat_ranks(T,NewIter,NewPrev,ChessBoard).

fix_spaces([],Same,Same):-
        !.

fix_spaces([H|T],Dummy,FixedPieces):-
        char_type(H, digit),
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

list_butlast([X|Xs], Ys) :-                 % use auxiliary predicate ...
   list_butlast_prev(Xs, Ys, X).            % ... which lags behind by one item

list_butlast_prev([], [], _). %SOURCE: https://stackoverflow.com/questions/16174681/how-to-delete-the-last-element-from-a-list-in-prolog
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-
   list_butlast_prev(Xs, Ys, X1).

