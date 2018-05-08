%%%-------------------------------------------------------------------
%%% @author Kasper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2018 15:12
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Kasper").

%% API
-export([start/0, stop/0, init/0]).

start() ->
  register(pollution_srv, spawn(?MODULE, init, [])).

stop() ->
  pollution_srv ! stop.

init() ->
  State = pollution:createMonitor(),
  loop(State).

loop(State) ->
  receive
    {add_station, {Name, Coordinates}} ->
      NewState = pollution:addStation(Name, Coordinates, State),
      case NewState of
        {error, exists} ->
          io:format("Station ~s : ~w already exists", [Name, Coordinates]),
          loop(State);
        _ ->
          loop(NewState)
      end;
    {add_value, {Identifier, Timestamp, Type, Value}} ->
      NewState = pollution:addValue(Identifier, Timestamp, Type, Value, State),
      case NewState of
        {error, exists} ->
          io:format("Value of type ~w and value ~w already eists in station ~w", [Type, Value, Identifier]),
          loop(State);
        {error, not_found} ->
          io:format("Station ~w not found", [Identifier]),
          loop(State);
        _ ->
          loop(NewState)
      end;
    {remove_value, {Identifier, Timestamp, Type}} ->
      NewState = pollution:removeValue(Identifier, Timestamp, Type, State),
      case NewState of
        {error, not_found} ->
          io:format("Station ~w not found", [Identifier]),
          loop(State);
        _ ->
          loop(NewState)
      end;
    {get_one_value, {Identifier, Timestamp, Type}} ->
      {_, _, Value} = pollution:getOneValue(Identifier, Type, Timestamp, State),
      case Value of
        {error, not_found} ->
          io:format("Station ~w not found", [Identifier]);
        _ ->
          io:format("Value of ~w in ~w from station ~w was: ~w", [Type, Timestamp, Identifier, Value])
      end,
      loop(State);
    {get_station_mean, {Identifier, Type}} ->
      StationMean = pollution:getStationMean(Identifier, Type, State),
      case StationMean of
        not_available ->
          io:format("Measurements of type ~w from station ~w not found", [Type, Identifier]);
        _ ->
          io:format("Station (~w) mean of ~w was: ~w", [Identifier, Type, StationMean])
      end,
      loop(State);
    {get_daily_mean, {Date, Type}} ->
      DailyMean = pollution:getDailyMean(Date, Type, State),
      case DailyMean of
        not_available ->
          io:format("Measurements of type ~w from date ~w not found", [Type, Date]);
        _ ->
          io:format("Daily mean of ~w in ~w was: ~w", [Type, Date, DailyMean])
      end,
      loop(State);
    {get_correlation, {Type1, Type2}} ->
      Correlation = pollution:getCorrelation(Type1, Type2, State),
      case Correlation of
        not_available ->
          io:format("Measurements not found", []);
        _ ->
          io:format("Correlation of ~w and ~w values is: ~w~n", [Type1, Type2, Correlation])
      end,
      loop(State);
    stop ->
      terminate()
  end.

terminate() ->
  ok.

