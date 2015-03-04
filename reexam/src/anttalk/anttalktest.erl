-module(anttalktest).


-import(anttalk,[ %% Ant API		  
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
-include_lib("eunit/include/eunit.hrl").

%test basic init of a colony		  
anttalk_initcolony_test() ->
	{ok,C} = anttalk:start(),
	?assert(is_pid(C)). 
	
%test init of an ant 
anttalk_initant_test() ->
	{ok,C} = anttalk:start(),
	{ok,A} = anttalk:new_ant(C),
	?assert(is_pid(A)). 	
	
	
	
%test init of multiple ants
anttalk_init5ants_test() ->
	{ok,C} = anttalk:start(),
	anttalk:new_ant(C),
	anttalk:new_ant(C),
	anttalk:new_ant(C),
	anttalk:new_ant(C),
	anttalk:new_ant(C),
	{ok,{AL,N}} = anttalk:ants(C),
	?assert((N==0) and (length(AL)==5)). 	
	
%test init of multiple ants and then kill one ant by sending wrong forward parameter
anttalk_init5antsAndKill1_test() ->
	{ok,C1} = anttalk:start(),
	anttalk:new_ant(C1),
	anttalk:new_ant(C1),
	anttalk:new_ant(C1),
	anttalk:new_ant(C1),
	{ok, A1} = anttalk:new_ant(C1),
	anttalk:forward(A1,-9),
	timer:sleep(1),
	{ok,{AL,N}} = anttalk:ants(C1),
	?assert((N==1) and (length(AL)==4)). 
	
%test init of multiple ants and then kill all ants by sending wrong 
%forward parameter , wrong setpen, left and right parameters
anttalk_init5antsAndKillAll_test() ->
	{ok,C} = anttalk:start(),
	{ok, A1} = anttalk:new_ant(C),
	{ok, A2} = anttalk:new_ant(C),
	{ok, A3} = anttalk:new_ant(C),
	{ok, A4} = anttalk:new_ant(C),
	{ok, A5} = anttalk:new_ant(C),
	anttalk:forward(A1,-9),
	anttalk:forward(A2,1.3),
	anttalk:setpen(A3,sdsaasd),
	anttalk:left(A4,45),
	anttalk:right(A5,92),
	timer:sleep(1),
	{ok,{AL,N}} = anttalk:ants(C),
	?assert((N==5) and (length(AL)==0)). 
	


%move ant and check new position
anttalk_moveAntAndCheckNewPosition_test() ->
	{ok,C} = anttalk:start(),
	{ok, A} = anttalk:new_ant(C),	
	timer:sleep(1),
	anttalk:forward(A,9),
	timer:sleep(1),
	anttalk:left(A,90),
	timer:sleep(1),
	anttalk:right(A,90),
	timer:sleep(1),
	anttalk:forward(A,9),
	timer:sleep(1),
	{ok,{X,Y}} = anttalk:position(A),
	?assert((X==0) and (Y==18)). 	
	

%test the functionality of clone function
anttalk_cloneAnts_test() ->
	{ok,C} = anttalk:start(),
	{ok, A} = anttalk:new_ant(C),	
	{ok, B} = anttalk:new_ant(C),
	timer:sleep(1),
	anttalk:clone(A,9),
	timer:sleep(1),
	anttalk:clone(B,1),
	timer:sleep(1),
	{ok,{AL,N}} = anttalk:ants(C),
	?assert((N==0) and (length(AL)==12)). 

%test that ant acutally dies
anttalk_killAnt_test() ->
	{ok,C} = anttalk:start(),
	{ok, A} = anttalk:new_ant(C),	
	S1 = is_process_alive(A),
	anttalk:forward(A,-2),
	timer:sleep(1),
	S2 = is_process_alive(A),
	timer:sleep(1),
	?assert(S1 and (not S2)). 
	
%test blast	
anttalk_blast_test() ->
	{ok,C} = anttalk:start(),
	{ok, A} = anttalk:new_ant(C),	
	S1 = is_process_alive(C),
	S2 = is_process_alive(A),
	anttalk:blast(C),
	timer:sleep(1),
	S3 = is_process_alive(C),
	S4 = is_process_alive(A),
	timer:sleep(1),
	?assert(S1 and S2 and (not S3) and (not S4)).
		
%move ant and check picture
anttalk_picture_test() ->
	{ok,C} = anttalk:start(),
	{ok, A} = anttalk:new_ant(C),	
	anttalk:setpen(A,up),
	timer:sleep(1),
	anttalk:forward(A,9),
	timer:sleep(1),
	anttalk:forward(A,9),
	timer:sleep(1),
	{ok,P} = anttalk:picture(C),
	?assert(length(P)==2). 	
	
%move ant without pen true and check picture
anttalk_picturewithoutpen_test() ->
	{ok,C} = anttalk:start(),
	{ok, A} = anttalk:new_ant(C),	
	anttalk:setpen(A,down),
	timer:sleep(1),
	anttalk:forward(A,9),
	timer:sleep(1),
	anttalk:forward(A,9),
	timer:sleep(1),
	{ok,P} = anttalk:picture(C),
	?assert(length(P)==0). 	
	
	
%move ant, kill ant and check graveyard
anttalk_graveyard_test() ->
	{ok,C} = anttalk:start(),
	{ok, A} = anttalk:new_ant(C),	
	anttalk:setpen(A,up),
	timer:sleep(1),
	anttalk:forward(A,9),
	timer:sleep(1),
	anttalk:forward(A,9),
	anttalk:forward(A,-9),
	timer:sleep(1),
	{ok,P} = anttalk:graveyard(C),
	?assert(length(P)==2). 		
	