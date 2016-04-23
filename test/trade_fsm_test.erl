-module(trade_fsm_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_fsm.hrl").

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
    {"The fsm is started propertly",
     ?setup(fun valid_initial_status/1)}.

ask_begin_session_test_() ->
    {"The fsm sends begin session message to client",
     ?setup(fun sends_negotiate_message/1)}.

%% Instantiators

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(trade_fsm))].

valid_initial_status(Pid) ->
    ?fsm_test(Pid, [{state,is,idle}]).

sends_negotiate_message(Pid) ->
    fun() ->
        OtherPid = self(),
        spawn(fun() -> trade_fsm:trade(Pid, OtherPid) end),
        Message = {'$gen_event',{ask_negotiate, Pid}},
        receive
            Message -> ok
        after 5000 ->
            erlang:error({receive_message_failed,
                          [{module, ?MODULE},
                           {line, ?LINE},
                           {expected, Message},
                           {value, empty}]})
        end
    end.
