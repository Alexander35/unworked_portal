-module(ets_suite).
-compile(export_all).

new(Name) ->
	ets:new(Name,[set, named_table]).

get_it(Name,Item) ->
	[{Item,List}]=ets:lookup(Name,Item),
	{Item, List}.

save_it(Name,Item,Stuff) ->
	ets:insert(Name,{Item,Stuff}).

