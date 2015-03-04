concat_Lists([],Res,Res).
concat_Lists([H|T], OtherList,[H | Rest]) :- concat_Lists(T, OtherList, Rest).