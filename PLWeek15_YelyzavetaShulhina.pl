my_reverse_oddonly(L1,L2) :- 
	my_rev_oddonly(L1,L2,[]).

my_rev_oddonly([X|CDRName],L2,Acc) :- 
	mod(X,2) =:= 0,
    my_rev_oddonly(CDRName,L2,Acc).

my_rev_oddonly([X|CDRName],L2,Acc) :- 
	mod(X,2) =\= 0,
    my_rev_oddonly(CDRName,L2,[X|Acc]).

my_rev_oddonly([],F,F) :- !.

output(W) :-
    my_reverse_oddonly(W, A),nl,write(A),nl.

:- initialization(output([1, 2, 3, 4, 5, 6, 7, 8])).
:- initialization(output([3, 5, 2, 3, 7, 1, 2, 3, 1, 8])).