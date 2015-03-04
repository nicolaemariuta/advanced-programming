bigger(elephant,horse).
bigger(horse,donkey).
bigger(donkey,dog).
bigger(donkey,monkey).

is_bigger(X, Y) :- bigger(X,Y).
is_bigger(X, Y) :- bigger(X,Z), is_bigger(Z,Y).

mortal(X) :- man(X).
man(socrates).

aunt(X, Z) :-
	sister(X, Y),	%a comment
	parent(Y,Z).
	
concat_lists([Elem | List1], List2, [Elem | List3]) :-
	concat_lists(List1, List2, List3).