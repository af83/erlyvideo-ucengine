-module(utils).

-export([get/2, get/3]).

get(Params, Key) when is_atom(Key) ->
    [Result] = get(Params, [Key]),
    Result;
get(Params, Keys) ->
    get(Params, Keys, none).
get(Params, Keys, Default) when is_atom(Default) ->
    get(Params, Keys, lists:map(fun(_Elem) ->
					Default
				end,
				Keys));
get(_Params, [], []) ->
    [];
get(Params, [Key|Keys], [Default|Defaults]) ->
    ValueList = case lists:keysearch(Key, 1, Params) of
		    {value, {Key, Value}} ->
			[Value];
		    false ->
			[Default]
		end,
    ValueList ++ ?MODULE:get(Params, Keys, Defaults).
