%%%-------------------------------------------------------------------
%%% @author Kuba Jano
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2018 12:16
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behavior(gen_server).
-define(SERVER, ?MODULE).
-author("Kuba Jano").

%% API
-export([start_link/0, init/1, addStation/1, get/0, addValue/1, removeValue/1, getOnValue/1, getStationMean/1, getDeviation/1, crash/0]).
-export([handle_cast/2, handle_call/3]).

start_link() ->  gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

%Klient - serwer
addStation({Name, Cords}) -> gen_server:cast(?SERVER, {addStat, Name, Cords}).
addValue({Key, Time, Type, Value}) -> gen_server:cast(?SERVER, {addVal, Key, Time, Type, Value}).
removeValue({Key, Time, Type}) -> gen_server:cast(?SERVER, {remVal, Key, Time, Type}).
getOnValue({Key, Time, Type}) -> gen_server:call(?SERVER, {getOn, Key, Time, Type}).
getStationMean({Key, Type}) -> gen_server:call(?SERVER, {getStat, Key, Type}).
getDeviation({Key, Type}) -> gen_server:call(?SERVER, {getDev, Key, Type}).
get() -> gen_server:call(?SERVER, get).
init(N) -> {ok, N}.
crash() -> 2/0.


%Callbacki
handle_cast({addStat, Name, Cords}, Monitor) -> NewMonitor=ok:addStation(Name, Cords, Monitor), {noreply, NewMonitor};
handle_cast({addVal, Key, Time, Type, Value}, Monitor) -> NewMonitor=ok:addValue(Key, Time, Type, Value, Monitor), {noreply, NewMonitor};
handle_cast({remVal, Key, Time, Type}, Monitor) -> NewMonitor=ok:removeValue(Key, Time, Type, Monitor), {noreply, NewMonitor}.

handle_call(get, _From, Monitor) -> {reply, Monitor, Monitor};
handle_call({getOn, Key, Time, Type}, _From, Monitor) -> ToReturn=ok:getOnValue(Key, Time, Type, Monitor), {reply, ToReturn, Monitor};
handle_call({getStat, Key, Type}, _From, Monitor) -> ToReturn=ok:getStationMean(Key, Type, Monitor), {reply, ToReturn, Monitor};
handle_call({getDev, Key, Type}, _From, Monitor) -> ToReturn=ok:getDeviation(Key, Type, Monitor), {reply, ToReturn, Monitor}.


