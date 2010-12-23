%%% @author     François de Metz <fdemetz@af83.com>
%%% @copyright  2010 af83
%%% @doc
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
-module(ucengine_ems_event).

-include("plugins/erlyvideo-ucengine/include/ucengine.hrl").
-include("erlyvideo.hrl").

-behaviour(gen_event).

-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

init(_Args) ->
    {ok, []}.

handle_event(#erlyvideo_event{event = stream_started,
			      stream_name = StreamName},
	     State) ->
    send_stream_event(StreamName, ?UCE_STREAM_START_EVENT),
    {ok, State};

handle_event(#erlyvideo_event{event =  stream_source_lost,
			      stream_name = StreamName},
	     State) ->
    send_stream_event(StreamName, ?UCE_STREAM_LOST_EVENT),
    {ok, State};

handle_event(#erlyvideo_event{event = stream_stopped,
			      stream_name = StreamName},
	     State) ->
    send_stream_event(StreamName, ?UCE_STREAM_STOP_EVENT),
    {ok, State};

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

%%--------------------------------------------------------------------
%% Private functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec send_stream_event(StreamName, Type) -> ok
%%
%% @doc send event to UC Engine about stream
%% @end
%%----------------------------------------------------------------------
send_stream_event(StreamName, Type) ->
    %% search last publisher
    {ok, [{StreamName, [{meeting, Meeting}, {uid, Uid}]}]} = ucengine_streams:lookup(StreamName),
    Event = #uce_event{type=Type,
		       location=[Meeting],
		       metadata=[{"broadcaster", Uid}]},
    %% push uce_event
    ucengine_client:publish(Event),
    ok.
