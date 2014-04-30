-module(event_exporter).

-export([start_link/0]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {
    last_export = undefined
}).

-define(INTERVAL, 10000). % 10 seconds

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, #s{}}.

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(trigger, State) ->
    tick(State),
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, State#s{last_export = erlang:now()}};
handle_info(Info, State) ->
    lager:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    tick(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick(_State) ->
    Metrics = folsom_metrics:get_metrics(),
    Values = get_values(Metrics),
    lager:info("[ event_exporter ] Got ~p", [Values]).

get_values(Metrics) ->
    get_values(Metrics, []).

get_values([], V) ->
    V;
get_values([H | T], V) ->
    get_values(T, [{H, folsom_metrics:get_metric_value(H)} | V]).
