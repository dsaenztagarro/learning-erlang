-module(trade_fsm).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_fsm).

-record(state, {name="",
                other,
                ownitems=[],
                otheritems=[],
                monitor,
                from}).

%% public API
-export([start/1, start_link/1, cancel/1, trade/2, idle/3, introspection_statename/1]).

%% gen_fms callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% public API

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Name], []).

cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

trade(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

introspection_statename(StateName) ->
    gen_fsm:sync_send_all_state_event(StateName,which_statename).

%% gen_fsm behaviour

init(Name) ->
    {ok, idle, #state{name=Name}}.

idle({negotiate, OtherPid}, From, S=#state{}) ->
    ask_negotiate(OtherPid, self()),
    {next_state, idle, S}.

idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

% handle_event(cancel, _StateName, S=#state{}) ->
%     notice(S, "received cancel event", []),
%     {stop, other_cancelled, S};

handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
    {stop, normal, ok, S};

handle_sync_event(which_statename, _From, StateName, LoopData) ->
    {reply, StateName, StateName, LoopData};

handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

terminate(normal, ready, S=#state{}) ->
    notice(S, "FSM leaving.", []);

terminate(_Reason, _StateName, _StateData) ->
    ok.

% helpers

% Send players a notice.
notice(#state{name=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).

%% Allows to log unexpected messages.
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [self(), Msg, State]).

ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).
