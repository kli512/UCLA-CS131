split_lists([], [], []).
split_lists([[Hx|Tx]|T1], [Hx|TFirsts], [Tx|TRest]) :-
    split_lists(T1, TFirsts, TRest).

transpose([[]|_], []).
transpose(M, [Heads|TrTail]) :-
    split_lists(M, Heads, Tails),
    transpose(Tails, TrTail).

constrain_list(N, L) :-
    fd_domain(L, 1, N),
    fd_all_different(L).

length_of(N, L) :- length(L, N).

count_list(_, C, [], Constraint) :-
    Constraint = C.
count_list(M, C, [H|T], Constraint) :-
    H > M ->
        CP1 is C + 1,
        count_list(H, CP1, T, Constraint);
    count_list(M, C, T, Constraint).

constrain_counts(N, T, TTr, C) :-
    counts(Top, Bottom, Left, Right) = C,

    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),

    % Checking Left - easiest first
    maplist(count_list(0, 0), T, Left),

    % Checking Right - just reverse each row of T
    maplist(reverse, T, TRev),
    maplist(count_list(0, 0), TRev, Right),

    % Checking Top - use TTr instead of T
    maplist(count_list(0, 0), TTr, Top),

    % Checking Bottom - Combine the previous - transpose and reverse T
    maplist(reverse, TTr, TTrRev),
    maplist(count_list(0, 0), TTrRev, Bottom).

tower(N, T, C) :-
    %%%% T Constraints %%%
    % Constraining number of rows and number of columns
    length(T, N),
    maplist(length_of(N), T),
    transpose(T, TTr),

    % Constraining row and column values
    maplist(constrain_list(N), T),
    maplist(constrain_list(N), TTr),

    %%% C Constraints %%%
    % Constraining count lengths and values
    maplist(fd_labeling, T),
    constrain_counts(N, T, TTr, C).

%%% Begin plain_list specific implementation %%%%

permute(N, Rows) :-
    findall(X, between(1, N, X), Range),
    findall(Y, permutation(Range, Y), Rows).

constrain_left_right([], _, [], []).
constrain_left_right([Row|TRows], Rows, [LeftSeen|TLeft], [RightSeen|TRight]) :-
    member(Row, Rows),
    reverse(Row, RowRev),
    count_list(0, 0, Row, LeftSeen),
    count_list(0, 0, RowRev, RightSeen),
    constrain_left_right(TRows, Rows, TLeft, TRight).

plain_tower(N, T, C) :-
    %%%% T Constraints %%%
    % Constraining number of rows and number of columns
    length(T, N),
    maplist(length_of(N), T),

    % Constraining C dimensions
    counts(Top, Bottom, Left, Right) = C,
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),

    % Constraining the matrix and count values
    permute(N, Rows),
    constrain_left_right(T, Rows, Left, Right),

    % Simply transposing to turn the Top and Bottom into Left and Right
    transpose(T, TTr),
    constrain_left_right(TTr, Rows, Top, Bottom).

%%% Ambiguous %%%

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.

%%% Speedup %%%

test_tower(Time) :-
    statistics(cpu_time, [Start|_]),
    findall(_, tower(4, _, _), _),
    statistics(cpu_time, [Stop|_]),
    Time is Stop - Start.

test_plain_tower(Time) :-
    statistics(cpu_time, [Start|_]),
    findall(_, plain_tower(4, _, _), _),
    statistics(cpu_time, [Stop|_]),
    Time is Stop - Start.

speedup(Speedup) :-
    test_tower(Tower),
    test_plain_tower(PlainTower),
    Speedup is PlainTower / Tower.

% Speedup2 - interestingly, plain_tower is much faster at finding a T that satisfies some C than tower is...

test_tower2(Time) :-
    statistics(cpu_time, [Start|_]),
    once(tower(5, _, counts([2,3,4,1,2], [3,2,1,4,2], [2,1,5,2,3], [2,4,1,4,2]))),
    statistics(cpu_time, [Stop|_]),
    Time is Stop - Start.

test_plain_tower2(Time) :-
    statistics(cpu_time, [Start|_]),
    once(plain_tower(5, _, counts([2,3,4,1,2], [3,2,1,4,2], [2,1,5,2,3], [2,4,1,4,2]))),
    statistics(cpu_time, [Stop|_]),
    Time is Stop - Start.

speedup2(Speedup) :-
    test_tower2(Tower),
    test_plain_tower2(PlainTower),
    Speedup is PlainTower / Tower.
