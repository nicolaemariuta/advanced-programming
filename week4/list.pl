% -*- Mode: Prolog -*-

% Extract the second element from a given list:
% ?- [a, b, c, d, e] = .



% Make sure the first element is a 1 and get the sub-list after the second element:
%?- MyList = [1, 2, 3, 4, 5], MyList = 

% Result should be:
% MyList = [1, 2, 3, 4, 5]
% Rest = [3, 4, 5]
% yes

sndRest(MyList, Rest) :- MyList = [1, _ | Rest ].

sndRestBetter([1, _ | Rest ], Rest). 

%?- [elephant, horse, tiger, dog] = [Head | Tail].



% We want to write a predicate concat_lists/3 to concatenate (append) two given lists.

% It should work like this:
% ?- concat_lists([1, 2, 3, 4], [dog, cow, tiger], L).
% L = [1, 2, 3, 4, dog, cow, tiger]
% yes
concat_lists([], Res, Res).
concat_lists([ H | T], OtherList, [ H | Rest]) :- concat_lists(T, OtherList, Rest).

