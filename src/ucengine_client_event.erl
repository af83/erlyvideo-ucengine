-module(ucengine_client_event).

-behaviour(gen_event).

-include("ucengine.hrl").
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
			location = [Org,Meeting],
			from = Uid}, State) ->
    case ucengine_client:can(Uid, "video", "view", [Org, Meeting], []) of
	true ->
	    Secret = ems:get_var(secret_key, "localhost", undefined), 
	    Token = json_session:encode([{"uid", Uid}], Secret),
	    Channel = json_session:encode([{"org", Org},
					   {"meeting", Meeting},
					   {"uid", Uid}], Secret),
	    Event = #uce_event{type=?UCE_STREAM_NEW_EVENT,
			       location=[Org,Meeting],
			       parent=Id,
			       to=Uid,
			       metadata=[{"token", binary_to_list(Token)},
					 {"channel", binary_to_list(Channel)}]},
	    %% push uce_event
	    io:format("PUB: ~p~n", [Event]),
	    ucengine_client:publish(Event),
	    {ok, State};
	_ ->
	    ems_log:error(default, "Action:uce_join_meeting_event (Org:~s/Meeting:~s) not authorized to ~s", [Org, Meeting, Uid]),
	    {error, State}
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
