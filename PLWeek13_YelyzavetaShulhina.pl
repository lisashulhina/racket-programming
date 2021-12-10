basel(0,0).

basel(N,B) :-
   N>0,
   N1 is N-1,
   basel(N1,B1),
   B is B1 + (1 / (N * N)).
