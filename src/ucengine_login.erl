%%% @author     François de Metz <fdemetz@af83.com>
%%% @copyright  2010 af83
%%% @doc        ucengine login
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
-module(ucengine_login).
-author('François de Metz <fdemetz@af83.com>').

-include("erlyvideo.hrl").

-include_lib("rtmp/include/rtmp.hrl").
-include("../../../include/rtmp_session.hrl").
-include("plugins/erlyvideo-ucengine/include/ucengine.hrl").

-export([connect/2, auth/3, publish/2, play/2]).

auth(_Host, _Protocol, _Session) ->
    [].

%%-------------------------------------------------------------------------
%% @spec connect(Session::rtmp_session(), Funcall::rtmp_funcall()) -> NewState::rtmp_session()
%% @doc Function always accept client, trusting all client information.
%% ACL on play or publis will be checked after
%% @end
%%-------------------------------------------------------------------------
connect(#rtmp_session{} = State, #rtmp_funcall{args = [_, SessionData|_]}) ->
    Secret = ems:get_var(secret_key, "localhost", undefined),
    Token = json_session:decode(binary_to_list(SessionData), Secret),
    NewState = State#rtmp_session{user_id = Token},
    %% TODO: we should check StreamName
    rtmp_session:accept_connection(NewState),
    NewState;

connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo, session_id = SessionId} = State, _AMF) ->
    ems_log:access(Host, "CONNECT ~s ~s ~p ~p ~s ~p trusted_login", [Address, Host, undefined, SessionId, proplists:get_value(pageUrl, PlayerInfo), []]),
    rtmp_session:accept_connection(State),
    State.

%%-------------------------------------------------------------------------
%% @spec play(Session::rtmp_session(), Funcall::rtmp_funcall()) -> NewState::rtmp_session()
%% @doc Function verify token and reject connection if acl return false
%%-------------------------------------------------------------------------
play(#rtmp_session{} = State, _) ->
    user_can(State, "view").

%%-------------------------------------------------------------------------
%% @spec publish(Session::rtmp_session(), Funcall::rtmp_funcall()) -> NewState::rtmp_session()
%% @doc Function verify token and reject connection if acl return false
%%
%% @end
%%-------------------------------------------------------------------------
publish(#rtmp_session{user_id = Token} = _State, #rtmp_funcall{args = [null, StreamName, Spec]}) when is_binary(Spec) ->
    case user_can(State, "publish") of
        unhandled ->
            ucengine_streams:insert(StreamName, Token),
            unhandled;
        S ->
            S
    end.

%%-------------------------------------------------------------------------
%% @spec user_can(Session::rtmp_session(), Funcall::rtmp_funcall()) -> NewState::rtmp_session()
%% @doc Function verify token with user acl
%% reject Connection if acl return false
%% @end
%%-------------------------------------------------------------------------
user_can(#rtmp_session{host = Host, user_id=[{org, Org}, {meeting, Meeting}, {uid, Uid}]} = State,Right) ->
    case ucengine_client:can(Uid, "video", Right, [Org, Meeting]) of
	true ->
            ems_log:access(Host, "check acl video.~s ok (org: ~s, meeting: ~s, uid: ~s)", [Right, Org, Meeting, Uid]),
            unhandled;
        _ ->
            ems_log:error(Host, "check acl video:~s nok (org: ~s, meeting: ~s, uid: ~s)", [Right, Org, Meeting, Uid]),
            rtmp_session:reject_connection(State)
    end.
