-module(facein).
-export([start/1,add_friend/2,friends/1,broadcast/3,received_messages/1]).

%start function which calls the loop and returns {ok, Pid}
start(N) -> {ok,  spawn(fun() -> loop(N, dict:new(), dict:new()) end)}.

%async and sync functions for making requests
async(Pid, Request) ->
	Pid ! {self(), Request}.

sync(Pid, Request) ->
	async(Pid, Request),
	receive
		{Pid, Response} -> Response
	end.

%request for adding a friend	
add_friend(P,F) ->
	sync(P, {add_friend,F}).

%display list with friends of P
friends(P) ->
	sync(P, friendsList).	

%process P broadcast message M with radius S	
broadcast(P, M, R) ->
	sync(P, {broadcast, M, R, make_ref()}).

%show all received messages	
received_messages(P) ->
	sync(P, received_messages).

%main loop using the parameters: user name, lists of messages and friends
loop(Name, FriendsList, MessageList)->
	receive 
		%sends friend request and process the answer
		{From, {add_friend,F}} ->
			R = sync( F, nameRequest),
			case R of
				{ok,N} -> async(From, ok),
						  loop(Name,dict:store(F, N, FriendsList), MessageList);
				{error, Reason} -> async(From, {error, Reason}),
								   loop(Name, FriendsList, MessageList)
			end;
		%answer to friend request
		{From, nameRequest} ->
			async(From, {ok, Name}),
			loop(Name,FriendsList, MessageList);
			
		%display the list of friends
		{From, friendsList} ->
			List = dict:to_list(FriendsList),
			async(From,{ok, List}),
			loop(Name, FriendsList, MessageList);
		
		%start broadcasting the message
		{From, {broadcast, M, R, Key}} ->
			List = dict:to_list(FriendsList),
			case R of
				0 -> async(From, ok),
					 loop(Name, FriendsList, dict:store(Key, {Name, M}, MessageList));
				_ -> async(From, ok), 
					 lists:map(fun({C, _}) -> async(C, {broadcastMessage, M, (R-1), Name, Key})end, List), 
					 loop(Name, FriendsList, dict:store(Key, {Name, M}, MessageList))
			end;
		%broadcast message is sent to friends list; only send further if count R is not 0	
		{From, {broadcastMessage, M, R, BroadcastName, Key}} ->
			List = dict:to_list(FriendsList),
			case dict:is_key(Key, MessageList) of
				false ->
					case R of
						0 -> loop(Name, FriendsList, dict:store(Key, {Name, M}, MessageList));
						_ -> lists:map(fun({C, _}) -> async(C, {broadcastMessage, M, (R-1), BroadcastName, Key})end, List), 
						loop(Name, FriendsList, dict:store(Key, {Name, M}, MessageList))
					end;
				true ->
					loop(Name, FriendsList, MessageList)
			end;
		%display all received messages
		{From, received_messages} ->
			List = dict:to_list(MessageList),
			async(From , {ok, lists:map(fun({_, C}) -> C end, List)}),
			loop(Name, FriendsList, MessageList)
	end.

