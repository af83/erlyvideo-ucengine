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
			      host = Host,
			      stream_name = Name }, State) ->

    Secret = ems:get_var(secret_key, "localhost", undefined),
    Channel = json_session:decode(Name, Secret),

    [Org64, Meeting64, Uid64] = re:split(Channel,":",[{return,list}]),
    Org = base64:decode_to_string(Org64),
    Meeting = base64:decode_to_string(Meeting64),
    Uid = base64:decode_to_string(Uid64),

    case ucengine_client:can(Uid, "video", "view", [Org,Meeting]) of
	true ->
	    Event = #uce_event{type=?UCE_STREAM_START_EVENT,
			       location=[Org,Meeting],
			       metadata=[{"stream_name",Name},
					 {"broadcaster",Uid}]},  
	    %% push uce_event
	    ucengine_client:publish(Event);
	_ ->
	    ems_log:error(Host, "Action:stream_started video:view (stream_name: ~s) not authorized to ~s", [Name, Uid]),
	    {error, State}
    end,
    {ok,State};

handle_event(#erlyvideo_event{event = stream_stopped,
			      host = _Host,
			      stream_name = Name},
	     State) ->
    Secret = ems:get_var(secret_key, "localhost", undefined),
    Channel = json_session:decode(Name, Secret),
    [Org, Meeting, Uid] = re:split(Channel,":",[{return,list}]),
    Event = #uce_event{type=?UCE_STREAM_STOP_EVENT,
		       location=[Org,Meeting],
		       metadata=[{"stream_name",Name},
				 {"broadcaster",Uid}]},
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
