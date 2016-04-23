-module(eunit_fsm).
-include_lib("eunit/include/eunit.hrl").
-export([translateCmd/2]).

translateCmd(Pid, {state,is,X}) ->
    {CurrentStateName, _CurrentStateData} = sys:get_state(Pid),
    ?assertEqual(X, CurrentStateName).

