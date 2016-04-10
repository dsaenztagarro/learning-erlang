-module(kitty_gen_server).
-behavior(gen_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-record(cat, {name, color=green, description}).


%% Client API

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).


%% Server functions

init([]) ->
    {ok, []}.

handle_call({order, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
        {reply, make_cat(Name, Color, Description), Cats};
       Cats =/= [] ->
        {reply, hd(Cats), tl(Cats)}
    end;

handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
    {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
    % io:format("Unexpected message"),
    {noreply, Cats}.

terminate(normal, Cats) ->
    ok.


%% Private functions

make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.
