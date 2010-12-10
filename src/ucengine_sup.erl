%%% @author     Thierry Bomandouki <thierry.bomandouki@af83.com> [http://ucengine.org]
%%% @copyright  2010 af83
%%% @doc        Functions to detect different kinds of media, according to their urls
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%%
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
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
