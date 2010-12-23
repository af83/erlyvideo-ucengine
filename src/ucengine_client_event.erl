%%% @author     Thierry Bomandouki <thierry.bomandouki@af83.com> [http://ucengine.org]
%%% @copyright  2010 af83
%%% @doc        ucengine login
%%% @end
%%%
%%% This file is part of erlyvideo-ucengine.
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
-module(ucengine_client_event).

-behaviour(gen_event).

-include("plugins/erlyvideo-ucengine/include/ucengine.hrl").
-include("erlyvideo.hrl").

-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

init(_Args) ->
    {ok, []}.

%%-------------------------------------------------------------------------
%% @spec (Event, State) -> {ok, NewState}            |
%%                         {ok, NewState, hibernate} |
%%                         {swap_handler,Args1,NewState,Handler2,Args2} |
%%                         remove_handler
%% @doc Callback for events.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_event(#uce_event{id=Id,
			type=?UCE_MEETING_JOIN_EVENT,
			location = [Meeting],
			from = Uid}, State) ->
    case ucengine_client:can(Uid, "video", "view", [Meeting], []) of
	true ->
	    Secret = ems:get_var(secret_key, "localhost", undefined),
	    Token = json_session:encode([{"meeting", Meeting},
                                         {"uid", Uid}], Secret),
	    Event = #uce_event{type=?UCE_STREAM_NEW_EVENT,
			       location=[Meeting],
			       parent=Id,
			       to=Uid,
			       metadata=[{"token", binary_to_list(Token)},
					 {"channel", Meeting}]},
	    %% push uce_event
	    ucengine_client:publish(Event),
	    {ok, State};
	_ ->
	    ems_log:error(default, "Action:uce_join_meeting_event (Meeting:~s) not authorized to ~s", [Meeting, Uid]),
	    {ok, State}
    end;

handle_event(_, State) ->
    {ok, State}.

handle_call(Request, State) ->
    {ok, Request, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
