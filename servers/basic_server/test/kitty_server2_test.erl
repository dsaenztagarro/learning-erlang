-module(kitty_server2_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

start_link_test_() ->
    {"The server can be started",
     ?setup(fun is_registered/1)}.

order_cat_test_() ->
    {"Returns the ordered cat when stock is empty",
     ?setup(fun order_cat_empty_stock/1)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() -> kitty_server2:start_link().

stop(Pid) -> kitty_server2:close_shop(Pid).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(kitty_server2))].

order_cat_empty_stock(Pid) ->
    Res = kitty_server2:order_cat(Pid, 'Drew', red, 'Red cat'),
    [?_assert(Res == {order, 'Drew', red, 'Red cat'})].

