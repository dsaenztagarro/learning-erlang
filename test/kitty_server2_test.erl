-module(kitty_server2_test).
-include_lib("eunit/include/eunit.hrl").

% Macro for creating fixtures
-define(setup(F), {setup,
                   fun() -> kitty_server2:start_link() end,
                   fun(Pid) -> kitty_server2:close_shop(Pid) end,
                   F}).

%% Tests Generators

start_link_test_() ->
    {"The server can be started",
     ?setup(fun is_registered/1)}.

order_cat_test_() ->
    [{"Returns the ordered cat when stock is empty",
      ?setup(fun order_cat_empty_stock/1)},
     {"Returns the cat in stock doesn't matter the cat requested",
      ?setup(fun order_cat_with_stock/1)}].

%% Instantiators

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(kitty_server2))].

order_cat_empty_stock(Pid) ->
    Res = kitty_server2:order_cat(Pid, 'Drew', red, 'Red cat'),
    [?_assert(Res == {cat, 'Drew', red, 'Red cat'})].

order_cat_with_stock(Pid) ->
    kitty_server2:return_cat(Pid, {cat, 'Simon', blue, 'Blue cat'}),
    Res = kitty_server2:order_cat(Pid, 'Drew', red, 'Red cat'),
    [?_assert(Res == {cat, 'Simon', blue, 'Blue cat'})].

