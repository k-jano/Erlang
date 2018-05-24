%%%-------------------------------------------------------------------
%%% @author Kuba Jano
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. maj 2018 15:27
%%%-------------------------------------------------------------------
-module(pollution_gen_server_supervisor).
-behaviour(supervisor).
-author("Kuba Jano").

%% API
-export([start_link/0, init/1]).

start_link() -> {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []), {ok, Pid}.

init(_InitValue) ->
  io:format("~p (~p) starting... ~n", [{?MODULE}, self()]),
  RestartStrategy = one_for_one,
  MaxRestarts = 2,
  MaxSeconds = 3,
  Flags = {RestartStrategy, MaxRestarts, MaxSeconds},

  Restart = permanent,
  Shutdown = brutal_kill,
  Type = worker,
  ChildSpecification = {genServerId, {pollution_gen_server, start_link, []}, Restart, Shutdown, Type, [pollution_gen_server]},

  {ok, {Flags, [ChildSpecification]}}.

