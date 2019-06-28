myflatten([H|T], X) :- is_list(H), append(H, T, R), myflatten(R, X).
myflatten([H|T], [H|X]) :- myflatten(T, X).
myflatten([], []).
