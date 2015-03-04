-module(erltest).
-export([move/2,qsort/1,contains/2]).

move(north,{X, Y}) -> {X, Y+1};
move(west, {X, Y}) -> {X-1, Y}.

qsort([]) -> [];
qsort([Pivot|Rest]) ->
	qsort([X || X <- Rest, X < Pivot])
	++ [Pivot] ++
	qsort([X || X <- Rest, X >= Pivot]).
	
contains(_, leaf) -> false;
contains(Key, {node, K, Left, Right}) ->
	if 	Key =:= K -> true;
		Key < K	  -> contains(Key, Left);
		Key > K	  -> contains(Key, Right)
	end.