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
    %% search last publisher
    {ok, [{StreamName, [{org, Org}, {meeting, Meeting}, {uid, Uid}]}]} = ucengine_streams:lookup(StreamName),
    Event = #uce_event{type=?UCE_STREAM_START_EVENT,
		       location=[Org,Meeting],
		       metadata=[{"broadcaster",Uid}]},
    %% push uce_event
    ucengine_client:publish(Event),
    {ok, State};

handle_event(#erlyvideo_event{event = stream_stopped,
			      stream_name = StreamName},
	     State) ->
    %% search last publisher
    {ok, [{StreamName, [{org, Org}, {meeting, Meeting}, {uid, Uid}]}]} = ucengine_streams:lookup(StreamName),
    Event = #uce_event{type=?UCE_STREAM_STOP_EVENT,
		       location=[Org,Meeting],
		       metadata=[{"broadcaster",Uid}]},
    %% push uce_event
    ucengine_client:publish(Event),
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
