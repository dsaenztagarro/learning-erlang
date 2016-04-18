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
-export([start/1, start_link/1, cancel/1]).

%% gen_fms callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Name], []).

% Cancel the transaction
cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%% gen_fsm behaviour

init(Name) ->
    debugMsg(">>> init"),
    {ok, idle, #state{name=Name}}.

idle(Event, Data) ->
    debugMsg(">>> idle"),
    unexpected(Event, idle),
    {next_state, idle, Data}.

% handle_event(cancel, _StateName, S=#state{}) ->
%     notice(S, "received cancel event", []),
%     {stop, other_cancelled, S};

handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

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

