revapp([], A, A).
revapp([X|L], A, F) :-
    revapp(L, [X|A], F).

fast_reverse(L, R) :-
    revapp(L, [], R).

rever(A, B, [A|Tail]) :-
    fast_reverse(A, B),
    member(B, [A|Tail]).

rever(A, B, [_, Tail]) :-
    rever(A, B, Tail).

rever(_, _, []) :- false.
