%%%%%%%%%%%%    INCLUDES   %%%%%%%%%%%%%

:- ['./chessboard_basics.prolog'].

%%%%%%%%%%%%   WEAK PAWNS   %%%%%%%%%%%%%%

%%%%%%% FIX %%%%%%%%    DIRECTION CHANGE AND NOT ISOLATED WEAKNESSES ('P' on B2  (<- WEAK) AND 'P' on A4)

identify_weak_pawns(ChessBoard, WeakPawnsWhite, WeakPawnsBlack):-
	identify_pawns_and_chains(ChessBoard, WhitePawns, BlackPawns, WhitePawnChains, BlackPawnChains), 
	find_pawn_islands(WhitePawns, BlackPawns, _WhiteIslandsNo, _BlackIslandsNo, WhiteIslands, BlackIslands),
	decide_weakness_in_chain(WhitePawnChains, white, WeakPawnsInWhiteChains),
	decide_weakness_in_chain(BlackPawnChains, black, WeakPawnsInBlackChains),
	find_isolated_in_islands(WhiteIslands, WeakPawnsInWhiteIslands),
	find_isolated_in_islands(BlackIslands, WeakPawnsInBlackIslands),
	append(WeakPawnsInWhiteChains, WeakPawnsInWhiteIslands, WeakPawnsWhite0),
	append(WeakPawnsInBlackChains, WeakPawnsInBlackIslands, WeakPawnsBlack0),
	sort(WeakPawnsWhite0, WeakPawnsWhite),
	sort(WeakPawnsBlack0, WeakPawnsBlack).

find_isolated_in_islands([], []):- !.

find_isolated_in_islands([NextIsland|RestIslands], [WeakPawn|RestWeakPawns]):-
	length(NextIsland, 1), !,
	extract_pawn(NextIsland, WeakPawn),
	find_isolated_in_islands(RestIslands, RestWeakPawns).

find_isolated_in_islands([_|T], WeakPawns):-
	find_isolated_in_islands(T, WeakPawns).

extract_pawn([[Rank, File]], [Rank, File]).


decide_weakness_in_chain([], _, []):- !.

decide_weakness_in_chain([NextChain|RestChains], Colour, [WeakPawn|RestWeakPawns]):-
	find_base(NextChain, Colour, WeakPawn),
	decide_weakness_in_chain(RestChains, Colour, RestWeakPawns).


find_base(NextChain, Colour, WeakPawn):-
	predsort(nthcompare(2), NextChain, NextChainOrdered), !,
	NextChainOrdered = [[FirstRank, FirstFile],[SecondRank, SecondFile]|_],
	find_direction([FirstRank, FirstFile], [SecondRank, SecondFile], Direction),
	get_to_the_bottom(NextChainOrdered, Colour, Direction, WeakPawn).

get_to_the_bottom([WeakPawn|RestPawns], white, downright, WeakPawn):-
	RestPawns = [], !.
	
get_to_the_bottom([_|T], white, downright, WeakPawn):-
	!, get_to_the_bottom(T, white, downright, WeakPawn).

get_to_the_bottom([WeakPawn|_], white, upright, WeakPawn):- !.

get_to_the_bottom([WeakPawn|_], black, downright, WeakPawn):- !.

get_to_the_bottom([WeakPawn|RestPawns], black, upright, WeakPawn):-
	RestPawns = [], !.

get_to_the_bottom([_|T], black, upright, WeakPawn):-
	!, get_to_the_bottom(T, black, upright, WeakPawn).

identify_pawns_and_chains(ChessBoard, WhitePawns, BlackPawns, WhitePawnChains, BlackPawnChains):-
	identify_all_pawns(ChessBoard, WhitePawns, BlackPawns),
	identify_pawn_chains(ChessBoard, WhitePawns, BlackPawns, WhitePawnChains, BlackPawnChains).

identify_pawn_chains(ChessBoard, WhitePawns, BlackPawns, WhitePawnChains, BlackPawnChains):-
	find_pawn_chains(ChessBoard, white, WhitePawns, WhitePawnChains0),
	find_pawn_chains(ChessBoard, black, BlackPawns, BlackPawnChains0),
	fix_pawn_chains(WhitePawnChains0, WhitePawnChains),
	fix_pawn_chains(BlackPawnChains0, BlackPawnChains).

fix_pawn_chains(ChainsPrim, ChainsFinal):-
	sort_all_chains(ChainsPrim, ChainsPrimOrdered0),
	append(ChainsPrimOrdered0, ChainsPrimOrdered),
	sort(ChainsPrimOrdered, ChainsOrdered),  %%% TO REMOVE DUPLICATES
	length(ChainsOrdered, NoOfChains),
	remove_subset_chains(ChainsOrdered, 0, NoOfChains, ChainsFinal).

remove_subset_chains(_, Length, Length, []):- !.

remove_subset_chains(Chains, N, NoOfChains, ChainsFinal):-
	nth0(N, Chains, NthChain),
	is_subset(NthChain, Chains), !,
	NPlus is N + 1, 
	remove_subset_chains(Chains, NPlus, NoOfChains, ChainsFinal).

remove_subset_chains(Chains, N, NoOfChains, [NthChain|Tail]):-
	nth0(N, Chains, NthChain),
	NPlus is N + 1,
	remove_subset_chains(Chains, NPlus, NoOfChains, Tail).

is_subset(NthChain, [NthChain|T]):-
	!, is_subset(NthChain, T).

is_subset(NthChain, [NextChain|_]):-
	subset(NthChain, NextChain), !.

is_subset(NthChain, [_|T]):-
	is_subset(NthChain, T).
	
sort_all_chains([], []):- !.

sort_all_chains([[]|T], OrderedChains):-
	!, sort_all_chains(T, OrderedChains).

sort_all_chains([NextPawnsChains|RestPawnsChains], [NextPawnsChainsOrdered|RestOrderedChains]):-
	sort_all_chains_pawns(NextPawnsChains, NextPawnsChainsOrdered),
	sort_all_chains(RestPawnsChains, RestOrderedChains).

sort_all_chains_pawns([], []):- !.

sort_all_chains_pawns([NextChain|RestChains], [NextChainOrdered|RestChainsOrdered]):- 
	predsort(nthcompare(2), NextChain, NextChainOrdered), !,
	sort_all_chains_pawns(RestChains, RestChainsOrdered).

find_pawn_chains(_, _, [], []):- !.

find_pawn_chains(ChessBoard, Colour, [[Rank, 0]|RestPawns], [PawnChains|Tail]):-
	!, RankMinus is Rank - 1, 
	RankPlus is Rank + 1,
	possible_chain_squares(ChessBoard, Rank, 0, Colour, [[RankMinus, 1], [RankPlus, 1]], PawnChains),
	find_pawn_chains(ChessBoard, Colour, RestPawns, Tail).

find_pawn_chains(ChessBoard, Colour, [[Rank, 7]|RestPawns], [PawnChains|Tail]):-
	!, RankMinus is Rank - 1, 
	RankPlus is Rank + 1,
	possible_chain_squares(ChessBoard, Rank, 7, Colour, [[RankMinus, 6], [RankPlus, 6]], PawnChains),
	find_pawn_chains(ChessBoard, Colour, RestPawns, Tail).

find_pawn_chains(ChessBoard, Colour, [[Rank, File]|RestPawns], [PawnChains|Tail]):-
	RankMinus is Rank - 1, 
	RankPlus is Rank + 1, 
	FileMinus is File - 1,
	FilePlus is File + 1,
	possible_chain_squares(ChessBoard, Rank, File, Colour, [[RankMinus, FileMinus], [RankMinus, FilePlus], [RankPlus, FileMinus], [RankPlus, FilePlus]], PawnChains),
	find_pawn_chains(ChessBoard, Colour, RestPawns, Tail).

possible_chain_squares(_, _, _, _, [], []):- !.

possible_chain_squares(ChessBoard, Rank, File, Colour, [[TargetRank, TargetFile]|T], [PawnChain|Tail]):-
	return_piece_colour(pawn, Colour, Pawn),
	identify_piece([TargetRank, TargetFile], ChessBoard, Pawn), !,
	find_direction([Rank, File], [TargetRank, TargetFile], Direction),
	create_chain(ChessBoard, Rank, File, Colour, Direction, PawnChain),
	possible_chain_squares(ChessBoard, Rank, File, Colour, T, Tail).

possible_chain_squares(ChessBoard, Rank, File, Colour, [_|T], MyPawnChains):-
	possible_chain_squares(ChessBoard, Rank, File, Colour, T, MyPawnChains).
	 
create_chain(_, Rank, 7, _, upright, [[Rank, 7]]):- !.

create_chain(ChessBoard, Rank, File, Colour, upright, [[Rank, File]|Tail]):-
	NewRank is Rank - 1,
	NewFile is File + 1,
	return_piece_colour(pawn, Colour, Pawn),
	identify_piece([NewRank, NewFile], ChessBoard, Pawn), !, 
	create_chain(ChessBoard, NewRank, NewFile, Colour, upright, Tail).

create_chain(_, Rank, File, _, upright, [[Rank, File]]):- !.

create_chain(_, Rank, 0, _, upleft, [[Rank, 0]]):- !.

create_chain(ChessBoard, Rank, File, Colour, upleft, [[Rank, File]|Tail]):-
	NewRank is Rank - 1,
	NewFile is File - 1,
	return_piece_colour(pawn, Colour, Pawn), 
	identify_piece([NewRank, NewFile], ChessBoard, Pawn), !,
	create_chain(ChessBoard, NewRank, NewFile, Colour, upleft, Tail).

create_chain(_, Rank, File, _, upleft, [[Rank, File]]):- !.

create_chain(_, Rank, 7, _, downright, [[Rank, 7]]):- !.

create_chain(ChessBoard, Rank, File, Colour, downright, [[Rank, File]|Tail]):-
        NewRank is Rank + 1,
        NewFile is File + 1,
	return_piece_colour(pawn, Colour, Pawn),
        identify_piece([NewRank, NewFile], ChessBoard, Pawn), !,
        create_chain(ChessBoard, NewRank, NewFile, Colour, downright, Tail).

create_chain(_, Rank, File, _, downright, [[Rank, File]]):- !.

create_chain(_, Rank, 0, _, downleft, [[Rank, 0]]):- !.

create_chain(ChessBoard, Rank, File, Colour, downleft, [[Rank, File]|Tail]):-
        NewRank is Rank + 1,
        NewFile is File - 1,
	return_piece_colour(pawn, Colour, Pawn),
        identify_piece([NewRank, NewFile], ChessBoard, Pawn), !,
        create_chain(ChessBoard, NewRank, NewFile, Colour, downleft, Tail).

create_chain(_, Rank, File, _, downleft, [[Rank, File]]):- !.

sortIsland([],_, []).

sortIsland([NextIsland|RestIslands], white, [NextIslandSorted|RestSorted]):-
	predsort(nthcompare(1), NextIsland, NextIslandSorted0), !,
	reverse(NextIslandSorted0, NextIslandSorted),
	sortIsland(RestIslands, white, RestSorted).

sortIsland([NextIsland|RestIslands], black, [NextIslandSorted|RestSorted]):-
        predsort(nthcompare(1), NextIsland, NextIslandSorted), !,
        sortIsland(RestIslands, black, RestSorted).

find_pawn_islands(WhitePawns, BlackPawns, WhiteIslandsNo, BlackIslandsNo, WhiteIslands, BlackIslands):-
	group_pawns(WhitePawns, 0, WhiteIslands0),
	group_pawns(BlackPawns, 0, BlackIslands0),
	remove_empty_list_elements(WhiteIslands0, WhiteIslands1),
	remove_empty_list_elements(BlackIslands0, BlackIslands1),
	fix_islands(WhiteIslands1, WhiteIslands),
	fix_islands(BlackIslands1, BlackIslands),
	length(WhiteIslands, WhiteIslandsNo),
	length(BlackIslands, BlackIslandsNo).

fix_islands([],[]).

fix_islands([H|T], [Head|Tail]):-
	append(H, Head),
	fix_islands(T, Tail).

group_pawns([], _, []):- !.

group_pawns(Pawns, File, [Island|Rest]):-
	fill_island(Pawns, File, Next_File, RestPawns, Island),
	group_pawns(RestPawns, Next_File, Rest).

fill_island([], _, 8, [], []):- !.

fill_island([[Rank, File]|T], Id_File, NewId_File, [[Rank, File]|T], []):-
	Id_File < File, !, 
	NewId_File is Id_File + 1.

fill_island(Pawns, File, Next_File, RestPawns, [Pawns_on_File|RestIsland]):-
	pawns_on_file(Pawns, File, Pawns_on_File, PawnsLeft),
	FilePlus is File + 1,
	fill_island(PawnsLeft, FilePlus, Next_File, RestPawns, RestIsland).

pawns_on_file([], _, [], []):- !.

pawns_on_file([[Rank, File]|T], File, [[Rank, File]|T2], RestPawns):-
	!, pawns_on_file(T, File, T2, RestPawns).

pawns_on_file(Pawns, _, [], Pawns).

identify_all_pawns(ChessBoard, WhitePawns, BlackPawns):-
	find_all_pawns(ChessBoard, 0, WhitePawns0, BlackPawns0),
        %remove_empty_list_elements(WhitePawns0, WhitePawns1),
        %remove_empty_list_elements(BlackPawns0, BlackPawns1),
        append(WhitePawns0, WhitePawns2),
        append(BlackPawns0, BlackPawns2),
        sort(2, @=<, WhitePawns2, WhitePawns),
        sort(2, @=<, BlackPawns2, BlackPawns).


find_all_pawns(_, 8, [], []):- !.

find_all_pawns(ChessBoard, Rank, [WhitePawnsonRank|RestWhite], [BlackPawnsonRank|RestBlack]):-
	find_all_pawns_rank(ChessBoard, Rank, 0, WhitePawnsonRank, BlackPawnsonRank),
	NewRank is Rank + 1,
	find_all_pawns(ChessBoard, NewRank, RestWhite, RestBlack).

find_all_pawns_rank(_, _, 8, [], []):- !.

find_all_pawns_rank(ChessBoard, Rank, File, [[Rank, File]|RestWhite], BlackPawns):-
	identify_piece([Rank, File], ChessBoard, 'P'), !, 
	NewFile is File + 1,
	find_all_pawns_rank(ChessBoard, Rank, NewFile, RestWhite, BlackPawns).

find_all_pawns_rank(ChessBoard, Rank, File, WhitePawns, [[Rank, File]|RestBlack]):-
	identify_piece([Rank, File], ChessBoard, 'p'), !, 
	NewFile is File + 1,
	find_all_pawns_rank(ChessBoard, Rank, NewFile, WhitePawns, RestBlack).
	
find_all_pawns_rank(ChessBoard, Rank, File, WhitePawns, BlackPawns):-
	NewFile is File + 1,
	find_all_pawns_rank(ChessBoard, Rank, NewFile, WhitePawns, BlackPawns).

remove_empty_list_elements([],[]).

remove_empty_list_elements([[]|T], Result):-
	!, remove_empty_list_elements(T, Result).

remove_empty_list_elements([H|T], [H|Tail]):-
	remove_empty_list_elements(T, Tail).


