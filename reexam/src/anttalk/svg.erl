-module(svg).
-export([pictureToSvg/1,
		 write_to_file/1]).


%%--------------------------------------------------------------------
%% @doc Create a string that is a SVG image representing the given picture.
%%
%% @spec pictureToSvg(Pic :: Picture) -> string()
%%  Position = {integer(), integer()}
%%  LineSeg  = {Position, Position}
%%  Picture  = [LineSeg]
%%
%% @end
%%--------------------------------------------------------------------
pictureToSvg (Picture) ->
    Points = lists:append(tuple_to_list(lists:unzip(Picture))),
    {Minx,Maxy} = lists:foldl (fun({X1,Y1}, {X2,Y2}) -> 
                                       {min(X1, X2), max(Y1, Y2)} end,
                               hd(Points), tl(Points)),    
    Xdir = 1 + if Minx < 0 -> abs(Minx); true -> 0 end,
    Ydir = 1 + Maxy,
    Lines = lists:map(fun svgline/1, Picture),
    lists:flatten(["<svg xmlns=\"http://www.w3.org/2000/svg\">",
                   string_format("<g transform=\"translate(~B, ~B) scale(1,-1)\">~n", 
                                 [Xdir, Ydir]),
                 Lines,
                 "</g></svg>"]).
                          

svgline ({{X1,Y1}, {X2,Y2}}) ->
    string_format("<line style=\"stroke-width: 2px; stroke:black; fill:white\"\
          \ x1=\"~B\" x2=\"~B\" y1=\"~B\" y2=\"~B\" />~n" , [X1, X2, Y1, Y2]).

string_format(S, L) ->
    lists:flatten(io_lib:format(S, L)).
	
write_to_file(S) ->
file:write_file("/Picture.svg", io_lib:fwrite("~p.\n", [S])).	
