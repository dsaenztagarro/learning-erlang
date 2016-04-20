-module(trade_fsm_test).
-include_lib("eunit/include/eunit.hrl").

% Macro for creating fixtures
-define(setup(Instantiator), {setup,
                              fun setup/0,
                              fun cleanup/1,
                              Instantiator}).

% Setup and cleanup
setup() ->
    {ok, Pid} = trade_fsm:start_link("client1"), Pid.

cleanup(Pid) ->
    trade_fsm:cancel(Pid).

%% Tests Generators

start_link_test_() ->
    {"The fsm can be started",
     ?setup(fun is_registered/1)}.

started_properly_test_() ->
    {"The fsm is started propertly in idle status",
     ?setup(fun valid_initial_status/1)}.


%% Instantiators

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(trade_fsm))].

valid_initial_status(Pid) ->
    valid_status(Pid, idle).

%% Helpers

valid_status(Pid, StatusName) ->
    [?_assertEqual(StatusName, trade_fsm:introspection_statename(Pid))].


