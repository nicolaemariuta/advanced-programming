-module(exceptional_moves).
-export([move/2,ignore_invalid/2]).

move(north, {X, Y}) -> {X, Y+1};
move(west, {0, _}) -> throw(invalid_move);
move(west, {X, Y}) -> {X-1, Y}.

ignore_invalid(Dir,Pos) ->
	try move(Dir, Pos)
	catch
		invalid_move -> Pos
	end.