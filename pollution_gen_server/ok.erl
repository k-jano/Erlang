%%%-------------------------------------------------------------------
%%% @author Kuba Jano
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. kwi 2018 15:26
%%%-------------------------------------------------------------------
-module(ok).
-author("Kuba Jano").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOnValue/4, getStationMean/3, getDeviation/3]).
-record(station, {name, cords, data}).
-record(measurement, {time, desc, value}).

createMonitor() -> #{}.

addStation(Name, Cords, Monitor) ->
  try checkCords(Cords), checkName(Name), hasStation(Monitor, Name), hasStation(Monitor, Cords)
  catch
    throw:_ -> io:format("Can not add Station ~n")
  end,
  Station = #station{name = Name, cords = Cords, data = #{}},
  maps:put(Cords, Station, maps:put(Name, Station, Monitor)).

addValue(Key, Time, Type, Value, Monitor) ->
  try checkType(Type), hasRecord(Monitor, Key, Time, Type)
  catch
    throw:_ -> io:format("Can not add Value ~n")
  end,
  {station, Name, Cords, Data} = maps:get(Key, Monitor),
  Record = #measurement{time = Time, desc = Type, value = Value},
  Station = #station{name = Name, cords = Cords, data = maps:put({Time, Type}, Record, Data)},
  maps:update(Name, Station, maps:update(Cords, Station, Monitor)).

removeValue(Key, Time, Type, Monitor) ->
  try
    {station, Name, Cords, Data} = maps:get(Key, Monitor),
    Station = #station{name = Name, cords = Cords, data = maps:remove({Time, Type}, Data)},
    maps:update(Name, Station, maps:update(Cords, Station, Monitor))
  catch
    error:{badkey, Key} -> io:format("There is no measurments~n")
  end.

getOnValue(Key, Time, Type, Monitor) ->
  try
    {station, _, _, Data}=maps:get(Key, Monitor),
    element(4, maps:get({Time, Type}, Data))
  catch
    error:{badkey, Key}  -> io:format("There is no station");
    error:{badkey, {Time, Type}} -> io:format("There are no measurments")
  end .

getStationMean(Key, Type, Monitor) ->
  try
    {station, _, _, Data}=maps:get(Key, Monitor),
    List=lists:filter(fun(X) ->case element(3, X) of Type -> true; _ -> false end end, maps:values(Data)),
    case length(List) of
      0->throw("There are no measurments");
      Size-> Sum =lists:foldl(fun(X, Acc)-> element(4, X) + Acc end, 0, List),
        Sum / Size
    end

  catch
    error:{badkey, Key}  -> io:format("There is no station");
    throw:_ -> io:format("Can not get Station Mean")
  end .


getDeviation(Key, Type, Monitor) ->
  try
    {station, _, _, Data}=maps:get(Key, Monitor),
    List=lists:filter(fun(X) ->case element(3, X) of Type -> true; _ -> false end end, maps:values(Data)),
    case length(List) of
      0->throw("There are no measurments");
      Size-> Avg =getStationMean(Key, Type, Monitor), Sum =lists:foldl(fun(X, Acc)-> (element(4, X)-Avg)*(element(4, X)-Avg) + Acc end, 0, List),
        Score=math:sqrt((Sum/Size)), Score
    end

  catch
    error:{badkey, Key}  -> io:format("There is no station");
    throw:_ -> io:format("Can not get Deviation")
  end .


% Helpers


checkCords(Cords) ->
  case Cords of
    {_, _} -> true;
    _ -> throw({badarg, "Invalid Cords argument.."})
  end.

checkName(Name) ->
  case is_list(Name) of
    true -> true;
    _ -> throw({badarg, "Invalid Name argument.."})
  end.

checkType(Type) ->
  case Type of
    "PM10" -> true;
    "PM2.5" -> true;
    "temp" -> true;
    _ -> throw({badarg, "Invalid measurement type.."})
  end.

hasStation(Monitor, Key) ->
  Res = lists:any(fun(X) -> case X =:= Key of true -> true; _ -> false end end, maps:keys(Monitor)),
  case Res of
    false -> false;
    true -> throw({badarg,"Station has been already registered in monitor.."})
  end.

hasRecord(Monitor, Key, Time, Type) ->
  try {station,_,_, Data} = maps:get(Key, Monitor), maps:get({Time, Type}, Data) of
    {measurement, Time, Type, _} -> throw("There is already a record of similar measurement..");
    _ -> false
  catch
    throw:_ -> throw("Unexpected <thrown> error  - record update..");
    error:{badkey, Key} -> throw("There is no station");
    error:{badkey, {Time, Type}} -> false;
    error:_ -> throw("Unexpected <erl> error - record update..")
  end.


