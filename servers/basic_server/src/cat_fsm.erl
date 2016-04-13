-module(cat_fsm).
-export([start/0, event/0]).

start() ->
    spawn(
