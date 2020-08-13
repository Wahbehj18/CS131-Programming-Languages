% CS131 - Homework 4 - Prolog

% ===========================================================
% ===== TAKEN FROM HINT CODE ================================
% ===========================================================

% https://github.com/CS131-TA-team/UCLA_CS131_CodeHelp/blob/master/Prolog/sudoku_cell.pl
% https://github.com/CS131-TA-team/UCLA_CS131_CodeHelp/blob/master/Prolog/plain_domain.pl

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% a list contain N elements 
% http://www.gprolog.org/manual/html_node/gprolog033.html
% http://www.gprolog.org/manual/gprolog.html#hevea_default674
% Domain is all the enumerated answers of between(1, N, X)
within_domain(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

% fill in a 2D array with lists of fixed length (N)
% http://www.gprolog.org/manual/gprolog.html#sec215
fill_2d([], [], _).
fill_2d([Head | Tail], [TransposeHead | TransposeTail], Domain):-
    permutation(Domain, Head),
    permutation(Domain, TransposeHead),
    fill_2d(Tail, TransposeTail, Domain).

% ===========================================================
% ===== TOWER FD ============================================
% ===========================================================

sub_length([],_).
sub_length([Head|Tail], N):- length(Head, N), sub_length(Tail, N). 

tower_constraints(_, []).
tower_constraints(N, [Head|Tail]) :-
    % constrain each lists length, domain, and force distinct values
    length(Head, N),
    fd_all_different(Head),
    fd_domain(Head, 1, N),
    tower_constraints(N, Tail).

tower(N, T, C) :-

    % apply constraints to T using tower_constraints and transpose
    length(T, N),
    sub_length(T, N),
    tower_constraints(N, T),
    transpose(T, TTranspose),
    tower_constraints(N, TTranspose),

    maplist(fd_labeling, T),
    C = counts(Top, Bottom, Left, Right),
    
    % top & bottom use transpose
    count_all_rows(TTranspose, Top),
    maplist(reverse, TTranspose, TTransReverse),
    count_all_rows(TTransReverse, Bottom),

    % left & right use original
    count_all_rows(T, Left),
    maplist(reverse, T, TReverse),
    count_all_rows(TReverse, Right).

count_all_rows([], []).
count_all_rows([Row|RowTail], [Count|CountTail]) :-
    count_row(Row, 0, Count, 0),
    count_all_rows(RowTail, CountTail).

count_row([], _, Count, RetCount) :-
    RetCount = Count.
count_row([Tower|TowerTail], TallestTower, Count, RetCount) :-
    TallestTower > Tower ->
    count_row(TowerTail, TallestTower, Count, RetCount);
    NewRetCount is RetCount + 1,
    count_row(TowerTail, Tower, Count, NewRetCount).

% ===========================================================
% ===== PLAIN TOWER =========================================
% ===========================================================

% Helper functions from discussion slides by Kimmo
% https://piazza.com/class_profile/get_resource/k52xkkxhhjs1i2/k6mlb7d7iwz3nz

elements_between(List, Min, Max) :-
    maplist(between(Min,Max), List).

all_unique([]).
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([_|T]) :- all_unique(T).

unique_list(N, List) :-
    length(List, N),
    elements_between(List, 1, N),
    all_unique(List).

plain_unique_rows([],_). 
plain_unique_rows([Head|Tail], N):- 
    maplist(between(1, N), Head),
    all_unique(Head), 
    plain_unique_rows(Tail, N).

plain_tower(N, T, C) :-

    length(T, N),
    sub_length(T,N), 
    transpose(T, TTranspose),

    within_domain(N, Domain), 
    fill_2d(T, TTranspose, Domain), 
    plain_unique_rows(T,N), 
    plain_unique_rows(TTranspose,N), 

    C = counts(Top, Bottom, Left, Right),
    
    % top & bottom use transpose
    count_all_rows(TTranspose, Top),
    maplist(reverse, TTranspose, TTransReverse),
    count_all_rows(TTransReverse, Bottom),

    % left & right use original
    count_all_rows(T, Left),
    maplist(reverse, T, TReverse),
    count_all_rows(TReverse, Right).

% ===========================================================
% ===== TIME STATISTICS =====================================
% ===========================================================

get_time(TowerTime) :-
    statistics(cpu_time, [Start|_]),
    tower(4, _, counts([3,2,1,3],[2,3,2,1],[3,2,1,3],[2,3,2,1])),
    tower(4, _, counts([3,2,1,3],[2,3,2,1],[3,2,1,3],[2,3,2,1])),
    tower(4, _, counts([3,2,1,3],[2,3,2,1],[3,2,1,3],[2,3,2,1])),
    tower(4, _, counts([3,2,1,3],[2,3,2,1],[3,2,1,3],[2,3,2,1])),
    tower(4, _, counts([3,2,1,3],[2,3,2,1],[3,2,1,3],[2,3,2,1])),

    statistics(cpu_time, [Stop|_]),
    TotalTowerTime is Stop - Start,
    TowerTime is TotalTowerTime/5.0.

get_plain_time(PlainTime) :-
    statistics(cpu_time, [PlainStart|_]),
    plain_tower(4, _, counts([3,2,1,3],[2,3,2,1],[3,2,1,3],[2,3,2,1])),

    statistics(cpu_time, [PlainStop|_]),
    PlainTime is PlainStop - PlainStart.

speedup(CPURatio) :-
    get_time(TowerTime), get_plain_time(PlainTime),
    CPURatio is float(PlainTime)/float(TowerTime).

% ==============================================
% ===== AMBIGUOUS ==============================
% ==============================================
ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.