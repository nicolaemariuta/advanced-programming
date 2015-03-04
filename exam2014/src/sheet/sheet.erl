-module(sheet).

%% API
-export([ sheet/0
        , cell/2]
      %  , add_viewer/2
      %  , remove_viewer/2
      %  , get_viewers/1
      %  , set_value/2]
		).

%%%===================================================================
%%% API
%%%===================================================================
sheet() -> {ok, spawn(fun() -> sheetServer( dict:store(atoms, dict:new(), dict:new())) end)}.

cell(S, A) -> 
{ok, BooleanValue} = check_atom_existance(S, A),
	case BooleanValue of 
		true -> 
			ReturnValue = get_cell_pid(S,A),
			case ReturnValue of 
				{ok,CellServer} ->
					{ok,CellServer};
				{error} ->
					{error,"No Cell server found"}
			end;
		false ->
			CellServer = spawn (fun() -> cellServer(dict_store(atom, A, 
													dict_store(sheet, S,
													dict_store(status,undefined,
													dict_store(value,undefined,
													dict_store(formula,undefined,
													dict_store(viewers,[],
													dict_store(dependencies,[],
													dict_store(watchers,[],
													dict:new()))))))))) end),
		rpc(S,{add_new_cell_server, A, CellServer}),
		{ok,CellServer}
	end.
	
%Required Functions
add_viewer(C,P) ->
	rpc(C,{add_viewer_pid, P}.
	
remove_viewer(C,P) ->
	rpc(C,{remove_viewer_pid,P}).
	
get_viewers(C)) ->
	rpc(C,{get_viewer_list}).
	
set_value(C,V) ->
	case V of
		{formula, F, Deps} ->
			async(C,{set_Value, {formula, F, Deps}});
		_ -> 
			async(C, {set_Value, {def, V}})
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


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
	
%% Helper functions

check_atom_existance(S,A) ->
	rpc(S,{check_atom,A}).
	
get_cell_pid(S,A) ->
	rpc(S,{get_cell_pid,A}).
	
set_watcher(C,CPid)	->
	async(C,{set_watcher,CPid}).
	
check_cell_defined(C) ->
	rpc(C,{is_defined}).
	
	
errors(F) ->
	try F() of 
		_ -> ok
	catch 
		error:Error -> {error,caught,Error}
	end.
	
	
exits(F) ->
	try F() of 
		_ -> ok
	catch 
		exit:Exit -> {exit,caught,Exit}
	end.	

%%% Server loops
sheetServer(SheetDatabase) ->
	receive
		{From, {check_atom, A}} ->
			{ok, AtomsDict} = dict:find(atoms,SheetDatabase),
			case dic:is_key(A,AtmosDict) of
				true -> 
					reply(From, {ok,true});
				false ->
					reply(From, {ok,false})
			end,
			sheetServer(SheetDatabase);
		{From, {add_new_cell_server, Atom, CellServer}} ->
			{ok,AtomsDict} = dict:find(atoms,SheetDatbase),
			NewAtomsDict = dict:store(Atom,CellServer,AtomsDict),
			NewSheetDatabase = dict:store(atoms,NewAtomsDict, Atomsdict),
			NewCellSheetDatabase = dict:store(sheet, self(), NewSheetDatabase),
			reply(From,{ok,added}),
			sheetServer(NewCellSheetDatabase);
		{From, {get_cell_pid,A}} ->
			{ok, AtomsDict} = dict:find(atoms,SheetDatabase)
			case dict:is_key(A,AtomsDict) of
				true -> 
					{ok,CellPid} = dict:find(A,AtomsDict),
					reply(From ,{ok, CellPid});
				false ->
					reply(From,{error})
			end,
			sheetServer(SheetDatabase)
	end.
	
cellServer(CellDatabase) ->
	receive
		{From, {add_viewer_pid,VPid}} ->
			{ok, ViewerList} = dict:find(viewers,CellDatabase),
			case lists:member(VPid, ViewerList) of
				true ->
					reply(From, {error, viewer_already_added}),
					cellServer(CellDatabase);
				false ->
					NewViewerList = [VPid, ViewerList],
					NewCellDatabase = dict:store(viewers, NewViewerList, CellDatabase),
					reply(From, {ok, viewer_added}),
					cellServer(NewCellDatabase)
			end;
		{From, {remove_viewer_pid, VPid}} ->
			{ok, ViewerList} = dict:find(viewers, CellDatabase),
			NewViewerList = lists:delete(VPid, ViewerList),
			NewCellDatabase = dict:store(viewers, NewViewerList, CellDatabase),
			reply(From, {ok, viewer_deleted}),
			cellServer(NewCellDatabase);
		{From, {get_viewer_list}} ->
			{ok, ViewerList} = dict:find(viewers, CellDatabase),
			reply(From, {ViewerList}),
			cellServer(CellDatabase);
		{From, {set_watcher, CPid}} ->
			{ok, WatcherList} = dict:find(watchers, CellDatabase),
			NewWatcherList = [CPid | WatcherList}],
			NewCellDatabase = dict:store(watchers, NewWatcherList, CellDatabase),
			cellServer(NewCellDatabase);
		{set_value, {def, Value}} ->
			NewCellDatabase = dict:store(value, Value, CellDatabase),
			{ok, ViewerList} = dict:find(viewers, NewCellDatabase),
			{ok, Name} = dict:find(atom, NewCellDatabase),
			lists:map(fun(ViewerPid) -> async (ViewerPid, {update,
														   Name,
														   {def, Value},
														   dependency_list})  end, ViewerList),
			cellServer(NewCellDatabase);
		{set_value, {formula, F, Deps}} ->
			NewFormulaCellDatabase = dict:store(formula, F, CellDatabase),
			NewDListCellDatabase = dict:store(dependencies, Deps, NewFormulaCellDatabase),
			{ok, SheetPID} = dict:find(sheet, CellDatabase),
			lists:map(fun(CellAtom) -> errors({ok,CellPid} = get_cell_pid(SheetPID, CellAtom)),
											  set_watcher(CellPid, self())
											  end, Deps),
			cellServer(NewDListCellDatabase);
		{update, Name, {def, Val}, _} ->
			cellServer(CellDatabase);
			
			
	end.

