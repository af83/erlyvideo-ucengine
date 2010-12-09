%%%---------------------------------------------------------------------------------------
%%% @author     Thierry Bomandouki <thierry.bomandouki@af83.com> [http://erlyvideo.org]
%%% @copyright  2010 Thierry Bomandouki 
%%% @doc        erlyvideo custom login for ucengine plugin 
%%% @reference  See <a href="http://ucengine.org" target="_top">http://ucengine.org</a> for more information
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
-module(ucengine_check).
-author('Thierry Bomandouki <thierry.bomandouki@af83.com>').

-include("../include/ucengine.hrl").
-include("../include/rtmp_session.hrl").
%-include("../../../deps/rtmp/include/rtmp.hrl").
%-include("../../include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([connect/2]).

-export([getStreams/2, publish/2]).

-export(['WAIT_FOR_DATA'/2]).

'WAIT_FOR_DATA'({newStreamRegistered, StreamName}, #rtmp_session{socket = Socket} = State) ->
  io:format("New stream ~p~n", [StreamName]),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'newStream', args = [null, StreamName]}),
  {next_state, 'WAIT_FOR_DATA', State};


'WAIT_FOR_DATA'(_Message, #rtmp_session{} =_State) -> {unhandled}.

%%
%% Custom auth using ucengine plugin
%% 
connect(#rtmp_session{host = Host, addr = Address, socket = Socket, player_info = PlayerInfo} = State, _AMF) ->
  Secret = ems:get_var(secret_key, "localhost", undefined),
  PageUrl = proplists:get_value(pageUrl, PlayerInfo),
  {http,_,_Hostname,_Port,_Path,QueryString} = http_uri:parse(binary_to_list(PageUrl)),
  Params = [ list_to_tuple(re:split( re:replace(Param,"\\?","",[{return,list}]), "=", [{return,list}])) || Param <- re:split(QueryString,"&",[{return,list}])], 
  Channel = proplists:get_value("channel",Params),
  Token = proplists:get_value("token",Params),
  NewState = if
		Channel == undefined ; Token == undefined -> 
			rtmp_session:accept_connection(State),  
			State;
		true ->
			Secret = ems:get_var(secret_key, "localhost", undefined),
			[Org,Meeting,Uid] = re:split( json_session:decode(Channel,Secret),":",[{return,list}]),
			TUid = json_session:decode(Token,Secret),
			if
				Uid == TUid -> 
					%% check ACL


				Resp = none,
%				        Resp = httpc:request("http://"++ucengine_client:get("host")++":"++ucengine_client:get("port")++"/api/"++?UCE_API_VERSION++"/user/"++Uid++"/acl/video/view/"++Org++"/"++Meeting++"?uid="++?UCE_BRICK_ID++"&sid="++ucengine_client:get("sid")),	
					Authorized = case Resp of
							{ok, {{_,200,"OK"},_,JSONString}} ->
								{struct,[{"result",Value}]} = mochijson:decode(JSONString),
								case Value of
									"true" -> true;
									_ -> false
								end;
							_ -> false
						      end,
					case Authorized of	
						true ->
							UserId = Token,
							Channels = [Channel],	 
							{ok, SessionId} = ems_users:login(Host, UserId, Channels),
							TheState = State#rtmp_session{session_id = SessionId},
							ems_log:access(Host, "CONNECT ~p ~s ~p ~s ~p", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), self()]),
							rtmp_session:accept_connection(TheState),	
							rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'setId', args = [null, UserId]}),
							TheState;
						_ ->
							rtmp_session:accept_connection(State),
							State
					end;
				true ->
					rtmp_session:accept_connection(State),
					State
			end	
	     end,
  NewState.
	
	
getStreams(#rtmp_session{host = Host} = State, AMF) ->
  Streams = [Name || {Name, _} <- media_provider:entries(Host)],
  io:format("getStreams() -> ~p~n", [Streams]),
  rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, Streams]}),
  State.

publish(#rtmp_session{host = Host, path=Path} = State, #rtmp_funcall{args = [null,URL,_]} = AMF) ->
  Secret = ems:get_var(secret_key, "localhost", undefined),
  Name = "test",
  Channel = json_session:decode(Name, Secret),
  [Org,Meeting,Uid] = re:split(Channel,":",[{return,list}]), 
  %% check ACL

    Resp = none,
%  Resp = httpc:request("http://"++ucengine_client:get("host")++":"++ucengine_client:get("port")++"/api/"++?UCE_API_VERSION++"/user/"++Uid++"/acl/video/publish/"++Org++"/"++Meeting++"?uid="++?UCE_BRICK_ID++"&sid="++ucengine_client:get("sid")),
  Authorized = case Resp of
		{ok, {{_,200,"OK"},_,JSONString}} ->
			{struct,[{"result",Value}]} = mochijson:decode(JSONString),
			case Value of
				"true" -> true;
				_ -> false
			end;
		_ -> false
	       end,
  case Authorized of
	true ->  
	  State1 = apps_recording:publish(State, AMF),
	  {ok, Clients} = ems_users:clients(Host),
	  io:format("Notifying ~p~n", [Clients]),
	  [gen_fsm:send_event(Client, {newStreamRegistered, URL}) || Client <- Clients, Client /= self()],
	  State1;
	_ -> State
  end.
  
  
