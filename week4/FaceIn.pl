%list of persons  for testing FaceIn
g0([person(susan, [reed, jen, andrzej, jessica]),
      person(reed, [tony, jessica]),
      person(jessica, [jen]),
      person(tony, []),
      person(ken, [andrzej]),
      person(jen, [tony, susan, jessica]),
      person(andrzej, [susan,ken])]).
	  

%predicate that checks if variable X is part of a list	  
mymember(X,[X|_]).
mymember(X,[_|Tail]):-
	mymember(X,Tail).
	
%testing if it works to find if a person is in Graph by using only the name of that person
testMember(X) :-
	g0(G),
	mymember(person(X),G).	

%myselect(X,L1,L2) where list L2 is L1 without element X	
myselect(X,[X|L1],L1).
myselect(Y,[X|L1],[X|L2]):-
	myselect(Y,L1,L2).

%predicate for checking equality 	
myeq(X,X).

%subset(L1,L2). predicate that checks if L1 is included in L2
subset([],_).
subset([X|L1],L2) :-
	myselect(X,L2,L3),
	subset(L1,L3).

%predicat for checking if  persons X and Y are good friends in graph G
goodFriends(X,Y,G) :-
	mymember(person(X,W),G),    %at first check if X and Y are members of graph G 
	mymember(person(Y,V),G),	%and stores their lists of friends in W and V
	mymember(X,V),				%check if X and Y are in the lists of friends  of the other one
	mymember(Y,W).
	
%predicate for making easier to test goodFriends
testGoodFriends(X,Y) :-
	g0(G),
	goodFriends(X,Y,G).

%predicate clique
clique(G,L) :-
	clique2(G,L,L).

%predicate clique2 that has list L2 to store all persons
%and list L1 from which all persons are checked if list L2 is subset of their list of friends	
clique2(_,[],_).
clique2(G,[X|L1],L2):-
	mymember(person(X,W),G),
	myselect(X,L2,L3),
	subset(L3,W),
	clique2(G,L1,L2).

%predicate for checking if clique works properly	
testClique(L):-
	g0(G),
	clique(G,L).

%check if there is a transitive path between X and Y, starting from X	
ispath(X,Y,_):-
	myeq(X,Y).
ispath(X,Y,G):-
	myselect(person(X,W),G,G0),
	mymember(Z,W),
	ispath(Z,Y,G0).

%predicate for testing if ispath works	
testIspath(X,Y):-
	g0(G),
	ispath(X,Y,G).
%predicate wannabe removes the person X from the list G and then calls checkWannabe
wannabe(X,G):-
	myselect(person(X,_),G,G0),
	checkWannabe(X,G0,G).
%predicate 	that takes all persons from remaining list and check
% if there is path starting from X going to each person
checkWannabe(_,[],_).
checkWannabe(X,[person(Y,_)|G0],G):-
	ispath(X,Y,G),
	checkWannabe(X,G0,G).
%predicate for testing wannabe	
testWannabe(X):-
	g0(G),
	wannabe(X,G).

%predicate idol done almost same like wannabe
idol(X,G):-
	myselect(person(X,_),G,G0),
	checkIdol(X,G0,G).
%only that it checks if from each other person in graph is a path to X	
checkIdol(_,[],_).
checkIdol(X,[person(Y,_)|G0],G):-
	ispath(Y,X,G),
	checkIdol(X,G0,G).
%predicate for testing idol
testIdol(X):-
	g0(G),
	idol(X,G).

%predicate ispath which takes the list P with path from X to Y and 
%checks if it is valid path	
isPath(_,X,Y,_):-
	myeq(X,Y).
isPath(G,X,Y,[X,A,Z|P]):-
	isArrow(X,A,Z,G),
	isPath(G,Z,Y,[Z|P]).

%predicate for testing ispath	
testIspath(X,Y,P):-
	g0(G),
	isPath(G,X,Y,P).
	
%predicate if there is arrow -> between X and Y	
isArrow(X,->,Y,G):-
	member(person(X,W),G),
	member(person(Y,_),G),
	member(Y,W).
%predicate if there is arrow <- between X and Y		
isArrow(X,<-,Y,G):-
	member(person(X,_),G),
	member(person(Y,V),G),
	member(X,V).
%predicate for testing isArrow	
testIsArrow(X,A,Y):-
	g0(G),
	isArrow(X,A,Y,G).
