-module(faceinTest).
-import(facein,[start/1,add_friend/2,friends/1,broadcast/3,received_messages/1]).
-export([test1/0,test2/0]).
-include_lib("eunit/include/eunit.hrl").

%test that builds graph and displays friends of Jen
test1() -> 
	%create persons
	{ok,Ken} = facein:start(ken),
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
	facein:add_friend(Jen,Tony),
	
	%check friends
	facein:friends(Ken),
	facein:friends(Andrzej),
	facein:friends(Susan),
	facein:friends(Reed),
	facein:friends(Jessica),
	facein:friends(Tony),
	facein:friends(Jen).

%send broadcast messages and then display lists of messages 	
test2() -> 
	%create persons
	{ok,Ken} = facein:start(ken),
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
	facein:add_friend(Jen,Tony),
	
	%check friends
	facein:friends(Ken),
	facein:friends(Andrzej),
	facein:friends(Susan),
	facein:friends(Reed),
	facein:friends(Jessica),
	facein:friends(Tony),
	facein:friends(Jen),
	
	facein:broadcast(Jessica,"Message from jessica", 2),
	facein:broadcast(Ken,"Message from ken", 3),
	
	facein:received_messages(Tony),
	facein:received_messages(Susan),
	facein:received_messages(Reed).