start() -> spawn(fun() -> loop(dict:new()) end).

add(Pid, Contact) ->
	rpc(Pid, {add, Contact}).
	
list_all(Pid) ->
	rpc(Pid, list_all).
	
update(Pid, Contact) ->
	rpc(Pid, {update, Contact}).
	
	
loop(Contacts) ->
	receive
		{From, {add, Contact}} ->
			{Name,_,_} = Contact,
			case dict:is_key(Name, Contacts) of
				false ->
					From ! {self(), ok},
					loop(dict:store(Name, Contact, Contacts));
				true ->
					From ! {self(), {error, Name, is_already_there}},
					loop(Contacts)
			end;
			
			{From, list_all} ->
				List = dict:to_list(Contacts),
				From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
				loop(Contacts);
			{From, {update, Contact}} ->
				{Name,_,_} = Contact,
				NewContacts = dict:erase(Name, Contacts),
				From ! {self(), ok},
				loop(dict:store(Name, Contact, NewContacts));
			{From, Other} ->
				From ! {self(), {error,unknow_request, Other}},
				loop(Contacts)
end.

test() -> 
	%create persons
	{ok,Ken} = facein:start(ken),
	{ok,Andrzej} = facein:start(andrzej),
	{ok,Andrzej} = facein:start(andrzej),
	{ok,Susan} = facein:start(susan),
	{ok,Reed} = facein:start(reed),
	{ok,Jessica} = facein:start(jessica),
	{ok,Tony} = facein:start(tony),
	{ok,Jen} = facein:start(jen),
	%add friends
	facein:add_friend(Ken,Andrzej),
	facein:add_friend(Andrzej,Ken),
	facein:add_friend(Susan,Andrzej),
	facein:add_friend(Andrzej,Susan),
	facein:add_friend(Susan,Reed),
	facein:add_friend(Susan,Jessica),
	facein:add_friend(Reed,Tony),
	facein:add_friend(Reed,Jessica),
	facein:add_friend(Susan,Jen),
	facein:add_friend(Jen,Susan),
	facein:add_friend(Jessica,Jen),
	facein:add_friend(Jen,Jessica),
	facein:add_friend(Jen,Tony).