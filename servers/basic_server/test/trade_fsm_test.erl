-module(trade_fsm_test).
-include_lib("eunit/include/eunit.hrl").

% Macro for creating fixtures
-define(setup(F), {setup,
                   fun() -> trade_fsm:start_link() end,
                   fun(Pid) -> trade_fsm:cancel(Pid) end,
                   F}).

%% Tests Generators

start_link_test_() ->
    {"The fsm can be started",
     ?setup(fun is_registered/1)}.

%% Instantiators

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(kitty_server2))].


