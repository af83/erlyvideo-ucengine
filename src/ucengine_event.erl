%%% @author     Thierry Bomandouki <thierry.bomandouki@af83.com> [http://af83.com]
%%% @copyright  2010 AF83 
%%% @doc        Central point of erlyvideo events
%%% @reference  See <a href="http://ucengine.org/" target="_top">http://ucenegine.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(ucengine_event).
-author('Thierry Bomandouki <thierry.bomandouki@af83.com>').
-include_lib("../include/ucengine.hrl").
-include_lib("../include/erlyvideo.hrl").

%% External API

-export([start_link/0, notify/1]).

%% gen_event callbacks
-export([init/1]).

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),
    gen_event:add_handler(?MODULE, ucengine_client_event, []),
    gen_event:add_handler(?MODULE, ucengine_ems_event, []),
    {ok, Pid}.

notify(Event) ->
    gen_event:notify(?MODULE, Event).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_event
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (InitArgs) -> {ok, State}           |
%%                     {ok, State, hibernate}
%%
%% @doc Called by gen_event framework at process startup.
%% @end
%%----------------------------------------------------------------------

init([]) ->
    io:format("INIT~n"),
  {ok, []}.
