start() -> spawn(fun loop/0).
move(Pid, Dir, Pos) -> rpc(Pid, {Dir,Pos}).
rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.
loop() ->
	receive
		{From, {north, {X, Y}}} ->
			From ! {self(), {X, Y+1}},
			loop();
		{From, {west, {X, Y}}} ->
			From ! {self(), {X-1, Y}},
			loop();
		{From, Other} ->
			From ! {self(), {error, Other}},
			loop();
end.
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
			