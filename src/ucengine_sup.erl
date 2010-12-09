-module(ucengine_sup).
-author('Thierry Bomandouki <thierry.bomandouki@af83.com>').
-version(1.0).

-behaviour(supervisor).

-export([init/1,start_link/2]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end
%%--------------------------------------------------------------------
%-spec start_link(Host, Port) -> {'error',_} | {'ok',pid()}.
start_link(Host, Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port]).

init([Host, Port]) ->
    Supervisors = [{ucengine_event_sup,
		    {ucengine_event,start_link,[]},
		    permanent,
		    2000,
		    worker,
		    [ucengine_event]},
		   {ucengine_client_sup,
		    {ucengine_client, start_link, [Host, Port]},
		    permanent,
		    2000,
		    worker,
		    [ucengine_client]}],
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
