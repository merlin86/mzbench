%% Copyright (c) 2012 Campanja AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% A copy of the license is included in the file LICENSE.
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(graphite_client_sup).
-behaviour(supervisor).

-export([get_client/1]).
-export([start_link/0]).
-export([init/1]).

%% api
get_client(SelfPid) ->
    Host = get_env(graphite_host),
    case Host of
        undefined -> noclient;
        _ -> case start_client(SelfPid, Host, get_env(graphite_port)) of
                 {error, {already_started, Pid}} -> {ok, Pid};
                 {ok, Pid} -> {ok, Pid};
                 {error, Reason} -> {error, Reason}
             end
    end.

%% management api
start_link() -> supervisor:start_link(?MODULE, no_arg).

%% supervisor callback
init(no_arg) -> {ok, {{one_for_one, 5, 10}, []}}.


%% internal
start_client(SelfPid, Host, Port) ->
      supervisor:start_child(
        SelfPid,
        {graphite_client,
         {graphite_client, start_link, [Host, Port]},
         temporary,
         brutal_kill,
         worker,
         [graphite_client]}).

get_env(Name) ->
    case application:get_env(Name) of
        {ok, Value} -> Value;
        undefined -> undefined
    end.