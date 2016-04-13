-module(trade_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/0, event/0]).

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
