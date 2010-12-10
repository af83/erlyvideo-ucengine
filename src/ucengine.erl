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
-module(ucengine).
-author('Thierry Bomandouki <thierry.bomandouki@af83.com>').

-export([start/0, stop/0, start_ucengine/0]).

%% UCE PLUGIN API
start() -> 
    application:start(ucengine),
    ok.
  
stop() -> 
    application:stop(ucengine),
    application:unload(ucengine).

start_ucengine() ->
    ucengine_sup:start_ucengine().
