-module(kitty_server2_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

start_link_test() ->
    {"The server can be started",
     ?setup(fun is_registered/1)}.

order_cat_test() ->
    {"Returns the ordered cat when stock is empty",
     ?setup(fun order_cat_empty_stock/1)}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    {ok, Pid} = kitty_server2:start_link(),
    Pid.

stop(Pid) ->
    kitty_server2:close_shop(Pid).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(kitty_server2))].

order_cat_empty_stock(Pid) ->
    kitty_server2:order_cat(Pid, 'Drew', red, 'Red cat').
