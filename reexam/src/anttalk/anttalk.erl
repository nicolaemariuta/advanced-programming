-module(anttalk).

%% Exports
-export([ %% Ant API		  
          forward/2,
          left/2, 
          right/2,
          setpen/2,
		  clone/2,
          position/1,
         
          %% Colony API
		  start/0,
          blast/1,
          new_ant/1,
          picture/1,
          ants/1,
          graveyard/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% Ant API
forward(A,N) ->
	async(A, {moveForward, N}).
	
left(A,D) ->
	async(A, {rotateLeft, D}).
	
right(A,D) ->
	async(A, {rotateRight, D}).	
	
setpen(A,P) ->
	async(A, {set_pen, P}).
	

	
position(A) ->
	rpc(A, get_position).
	
	
	
clone(A, N) ->
	clones(A,N,[]).
	
%% Colony API
start() -> 
	{ok, spawn(fun() -> colony_loop([], 0, []) end)}.
	
blast(C) ->
	async(C, blast_colony).
	
new_ant(C) ->
	Pid = spawn(fun() -> ant_loop({0,0}, false, 0, [], C) end),
	async(C, {newAnt,Pid}),
	{ok,Pid}.

	
picture(C) ->
	rpc(C,get_picture).
	
ants(C) ->
	rpc(C,all_ants).
	
graveyard(C) ->
	rpc(C,get_graveyard).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Helping functions

%%recursively make all clones that are needed
clones(_,0,AL)  -> {ok,AL};
clones(A,N,AL) when n>0 -> {ok,L}  = rpc(A, clone_ant),
							clones(A,N-1,[L|AL]).

%%recursivley kill each ant process in the colony							
blastAllAnts(LivingAnts) -> blastAllAntsOneByOne(LivingAnts).


blastAllAntsOneByOne([]) -> stop;
blastAllAntsOneByOne([OneAnt|LivingAnts]) -> 
							async(OneAnt, stopAnt),
							blastAllAntsOneByOne(LivingAnts).

concatenateAllLivingAnts(AntIds) -> concatenateAllLivingAntsOneByOne(AntIds,[]).

concatenateAllLivingAntsOneByOne([],Picture) -> Picture;
concatenateAllLivingAntsOneByOne([Ant|AntIds],Picture) ->
												{ok,NextPic} = rpc(Ant,take_picture),
												NewPicture = NextPic++Picture,
												concatenateAllLivingAntsOneByOne(AntIds,NewPicture).
					

new_ant_clone(C,{X,Y},Angle) ->
	Pid = spawn(fun() -> ant_loop({X,Y}, false, Angle, [], C) end),
	async(C, {newAnt,Pid}),
	{ok,Pid}.


							
							
%%% Communication primitives

async(Pid, Msg) ->
    Pid ! Msg.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.
	
	
	
	
	
	


%%% Server loops

% Ant loop
ant_loop({X,Y},Pen,Angle, Picture, Colony) ->
	receive
		{moveForward, N} ->
			case is_integer(N) of
				false ->
					rpc(Colony,{ant_died,Picture}),
					io:format("Ant dead!");		
				true ->
					if 
						N < 0 ->
							rpc(Colony,{ant_died,Picture}),
							io:format("Ant dead!");
						true ->
							case Angle of
								0 -> 
									Mx = X,
									My = Y+N;
								90 ->
									Mx = X+N,
									My = Y;
								180 ->
									Mx = X,
									My = Y-N;
								270 ->
									Mx = X-N,
									My = Y
								end,
							if
								Pen == true ->
									Pts = [{{X,Y},{Mx,My}}|Picture],
									io:format("old picture: ~p",[Picture]),	
									ant_loop({Mx,My},true, Angle, Pts, Colony);
									
								true  ->
									ant_loop({Mx,My},Pen, Angle, Picture, Colony)
							end
									
						end
				end;
			
		{rotateLeft, D} ->
			if
				(D == 0) or (D == 90) or (D == 180) or (D == 270) ->
					M = Angle + D,
					A = M rem 360,
					ant_loop({X,Y},Pen, A, Picture, Colony);
				true ->
					rpc(Colony,{ant_died,Picture}),
					io:format("Ant dead!")
			end;
					
		{rotateRight, D} ->
			if
				(D == 0) or (D == 90) or (D == 180) or (D == 270) ->
					M = Angle - D + 360,
					A = M rem 360,
					ant_loop({X,Y},Pen, A, Picture, Colony);
				true ->
					rpc(Colony,{ant_died,Picture}),
					io:format("Ant dead!")
			end;
								
		{set_pen, P} ->
			case P of
				up ->
					ant_loop({X,Y},true, Angle, Picture, Colony);
				down ->
					ant_loop({X,Y},false, Angle, Picture, Colony);
				_ ->
					rpc(Colony,{ant_died,Picture}),
					io:format("Ant dead!")
			end;
		
		{From, get_position} ->
			reply(From, {ok, {X,Y}}),
			ant_loop({X,Y},Pen, Angle, Picture, Colony);
			
		{From, clone_ant} ->
			A = new_ant_clone(Colony,{X,Y},Angle),
			reply(From,{ok,A}),
			ant_loop({X,Y},Pen, Angle, Picture, Colony);
		stopAnt ->
			io:format("Ant dead!");
		{From, take_picture} ->
			reply(From,{ok,Picture}),
			ant_loop({X,Y},Pen, Angle, Picture, Colony)
			
	end.

% ant colony loop	
colony_loop (LivingAnts, NrDeadAnts, Graveyard) ->
	receive 
		blast_colony ->
			blastAllAnts(LivingAnts);
			
		{newAnt,Pid} ->
			NewLivingAnts = [Pid| LivingAnts],
			%io:format("ant added~p",[NewLivingAnts]),
			colony_loop(NewLivingAnts,NrDeadAnts, Graveyard);
			
		{From, get_picture} ->
			Pict = concatenateAllLivingAnts(LivingAnts),			
			reply(From, {ok,Pict}),			
			colony_loop(LivingAnts,NrDeadAnts, Graveyard);
		
		{From, all_ants} ->
			reply(From, {ok,{LivingAnts,NrDeadAnts}}),
			colony_loop(LivingAnts,NrDeadAnts, Graveyard);
			
		{From, get_graveyard} ->
			reply(From, {ok, Graveyard}) ,
			colony_loop(LivingAnts,NrDeadAnts, Graveyard);
			
		{From, {ant_died, DeadPicture}} ->
			%io:format("To the graveyard! ~p", [DeadPicture]),
			NewGraveyard = DeadPicture ++ Graveyard,
			NewNrDeadAnts = NrDeadAnts + 1,
			NewLivingAnts = LivingAnts -- [From],
			reply(From,{died}),
			io:format("To the graveyard! ~p", [NewGraveyard]),
			colony_loop(NewLivingAnts,NewNrDeadAnts, NewGraveyard)
			
	end.
