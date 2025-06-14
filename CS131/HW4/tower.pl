% Helpers
transpose([], []).
transpose([[] | _], []).
transpose(Matrix, [Head | Tail]) :- 
    maplist(head_tail_split, Matrix, Head, RestMatrix),
    transpose(RestMatrix, Tail).

head_tail_split([Head | Tail], Head, Tail).


count_visible(Row, Count) :-
    count_visible_helper(Row, 0, 0, Count).

count_visible_helper([], _, Count, Count).
count_visible_helper([Head|Tail], Prev, Count, Output) :- 
    (Head > Prev -> New_Count is Count + 1,
    count_visible_helper(Tail, Head, New_Count, Output);
    count_visible_helper(Tail, Prev, Count, Output)).

check_visible(T, T_Transposed, Top, Bottom, Left, Right) :-
    maplist(count_visible, T, Left), 
    maplist(reverse, T, T_Reversed),
    maplist(count_visible, T_Reversed, Right),
    maplist(count_visible, T_Transposed, Top),
    maplist(reverse, T_Transposed, T_Transposed_Reversed),
    maplist(count_visible, T_Transposed_Reversed, Bottom).


% ntower 
check_row_val(_, []).
check_row_val(N, [Head | Tail]) :-
    length(Head, N), 
    fd_domain(Head, 1, N), 
    fd_all_different(Head), 
    check_row_val(N, Tail).

ntower(N, T, counts(Top, Bottom, Left, Right)) :- 
    length(T, N),
    length(Top, N), 
    length(Bottom, N),
    length(Left, N), 
    length(Right, N),
    check_row_val(N, T),
    transpose(T, T_Transposed), 
    check_row_val(N, T_Transposed),
    maplist(fd_labeling, T), 
    check_visible(T, T_Transposed, Top, Bottom, Left, Right).


% plain_ntower 
check_diff([]).
check_diff([Head | Tail]) :-
    \+member(Head, Tail), 
    check_diff(Tail).

middle_of([], _, _).
middle_of([Head | Tail], Min, Max) :-
    between(Min, Max, Head), 
    middle_of(Tail, Min, Max).

plain_check_row_val(N, Row) :-
    length(Row, N), 
    middle_of(Row, 1, N), 
    check_diff(Row).

plain_ntower(N, T, counts(Top, Bottom, Left, Right)) :- 
    length(T, N),
    length(Top, N), 
    length(Bottom, N),
    length(Left, N), 
    length(Right, N),
    maplist(plain_check_row_val(N), T),
    transpose(T, T_Transposed), 
    maplist(plain_check_row_val(N), T_Transposed),
    check_visible(T, T_Transposed, Top, Bottom, Left, Right).


% speedup 
speedup(Ratio) :-
    statistics(runtime, [PlainStart|_]), 
    plain_ntower(4, _, counts([3, 2, 1, 2], [2, 2, 3, 1], [2, 2, 1, 3], [2, 2, 3, 1])),
    statistics(runtime, [PlainEnd|_]), 
    PlainTime is PlainEnd - PlainStart,

    statistics(runtime, [NTowerStart|_]), 
    ntower(4, _, counts([3, 2, 1, 2], [2, 2, 3, 1], [2, 2, 1, 3], [2, 2, 3, 1])),
    statistics(runtime, [NTowerEnd|_]), 
    NTowerTime is NTowerEnd - NTowerStart,

    Ratio is PlainTime / NTowerTime.


% ambiguous 
ambiguous(N, C, T1, T2) :-
    ntower(N, T1, C), 
    ntower(N, T2, C), 
    T1 \= T2.
