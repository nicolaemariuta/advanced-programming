is_bigger(X, Y) :- bigger(X,Y).
is_bigger(X, Y) :- bigger(X,Z), is_bigger(Z,Y).

is_bigger(elephant,horse).
is_bigger(horse,donkey).
is_bigger(donkey,dog).
is_bigger(donkey,monkey).