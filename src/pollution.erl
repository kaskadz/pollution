%%%-------------------------------------------------------------------
%%% @author Kasper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2018 21:38
%%%-------------------------------------------------------------------
-module(pollution).
-author("Kasper").

-include("../include/pollution.hrl").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getCorrelation/3]).

%% ====================
%% Public API functions
%% ====================

-spec createMonitor() ->
  monitor().
createMonitor() ->
  #monitor{}.

-spec addStation(station_name(), coords(), monitor()) ->
  monitor() | {error, exists}.
addStation(Name, Coordinates, Monitor) ->
  #monitor{stations = S, coords_to_station_name = CtSn} = Monitor,
  case stationExists(Name, Coordinates, Monitor) of
    false ->
      Station = #station{name = Name, coordinates = Coordinates},
      Monitor#monitor{
        stations = S#{Name => Station},
        coords_to_station_name = CtSn#{Coordinates => Name}
      };
    true -> {error, exists}
  end.

-spec addValue(id(), timestamp(), m_type(), float(), monitor()) ->
  monitor() | {error, exists} | {error, not_found}.
addValue(Identifier, Timestamp, Type, Value, Monitor) ->
  case getStation(Identifier, Monitor) of
    {ok, #station{measurements = Ms} = S} ->
      Measurement = createMeasurement(Timestamp, Type, Value),
      case measurementExists(Measurement, Ms) of
        true -> {error, exists};
        false ->
          updateStation(S#station{measurements = [Measurement | Ms]}, Monitor)
      end;
    {error, _} = Error ->
      Error
  end.

-spec removeValue(id(), timestamp(), m_type(), monitor()) ->
  monitor() | {error, not_found}.
removeValue(Identifier, Timestamp, Type, Monitor) ->
  case getStation(Identifier, Monitor) of
    {ok, #station{measurements = Ms} = S} ->
      RefMeasurement = createMeasurement(Timestamp, Type),
      Measurements = lists:filter(
        Ms,
        fun(X) ->
          not measurementEquals(X, RefMeasurement)
        end
      ),
      updateStation(S#station{measurements = Measurements}, Monitor);
    {error, _} = Error ->
      Error
  end.

-spec getOneValue(id(), m_type(), timestamp(), monitor()) ->
  measurement() | {error, not_found}.
getOneValue(Identifier, Type, Timestamp, Monitor) ->
  case getStation(Identifier, Monitor) of
    {ok, #station{measurements = Ms}} ->
      RefMeasurement = createMeasurement(Timestamp, Type),
      case lists:filter(Ms, fun(X) -> measurementEquals(X, RefMeasurement) end) of
        [Measurement] -> Measurement;
        [] -> {error, not_fonud}
      end;
    {error, _} = Error ->
      Error
  end.

-spec getStationMean(id(), m_type(), monitor()) ->
  float() | not_available.
getStationMean(Identifier, Type, Monitor) ->
  case getStation(Identifier, Monitor) of
    {ok, #station{measurements = Ms}} ->
      Series = lists:filtermap(
        fun({_Ts, T, V}) ->
          if
            T =:= Type -> {true, V};
            true -> false
          end
        end,
        Ms
      ),
      mean(Series);
    {error, _} = Error ->
      Error
  end.

-spec getDailyMean(calendar:date(), m_type(), monitor()) ->
  float() | not_available.
getDailyMean(Date, Type, #monitor{stations = S}) ->
  Stations = maps:values(S),
  AllMeasurements = lists:flatmap(
    fun(X) ->
      X#station.measurements
    end,
    Stations
  ),
  FilteredValues = lists:filtermap(
    fun({{Dt, _Tm}, T, V}) ->
      if
        (Dt =:= Date) and (T =:= Type) -> {true, V};
        true -> false
      end
    end,
    AllMeasurements
  ),
  mean(FilteredValues).

-spec getCorrelation(m_type(), m_type(), monitor()) ->
  float() | not_available.
getCorrelation(Type1, Type2, #monitor{stations = S} = Monitor) ->
  T1Measurements = fetchMeasurementsOfOneType(Type1, Monitor),
  T2Measurements = fetchMeasurementsOfOneType(Type2, Monitor),
  Avg1 = mean(T1Measurements),
  Avg2 = mean(T2Measurements),
  erlang:error(not_implemented).

%% ===================
%% Auxiliary functions
%% ===================

%%mergeMeasurements([{Ts1, V1} | T1], [{Ts2, V2} | T2]) ->


fetchMeasurementsOfOneType(Type, #monitor{stations = S}) ->
  Stations = maps:values(S),
  AllMeasurements = lists:flatmap(
    fun(X) ->
      X#station.measurements
    end,
    Stations
  ),
  lists:filtermap(
    fun({Ts, T, V}) ->
      case T of
        Type -> {true, {Ts, V}};
        _ -> false
      end
    end,
    AllMeasurements
  ).

-spec stationExists(station_name(), coords(), monitor()) ->
  boolean().
stationExists(Name, Coordinates, Monitor) ->
  case {getStation({name, Name}, Monitor), getStation({coords, Coordinates}, Monitor)} of
    {{error, not_found}, {error, not_found}} ->
      false;
    {{ok, _}, {ok, _}} ->
      true
  end.

-spec getStation(id(), monitor()) ->
  {ok, station()} | {error, not_found}.
getStation({name, Name}, #monitor{stations = S}) ->
  case maps:get(Name, S, none) of
    none -> {error, not_found};
    #station{} = S -> {ok, S}
  end;
getStation({coords, Coordinates}, #monitor{coords_to_station_name = CtSn} = M) ->
  case maps:get(Coordinates, CtSn, none) of
    none -> {error, not_found};
    Name -> getStation({name, Name}, M)
  end.

-spec measurementExists(measurement(), [measurement()]) ->
  boolean().
measurementExists(RefMeasurement, Measurements) ->
  lists:any(
    Measurements,
    fun(X) ->
      measurementEquals(X, RefMeasurement)
    end
  ).

-spec updateStation(station(), monitor()) ->
  monitor().
updateStation(Station, #monitor{stations = S} = M) ->
  M#monitor{
    stations = S#{
      Station#station.name := Station
    }
  }.

-spec createMeasurement(timestamp(), m_type()) ->
  measurement().
createMeasurement(Timestamp, Type) ->
  {Timestamp, Type, 0.0}.

-spec createMeasurement(timestamp(), m_type(), float()) ->
  measurement().
createMeasurement(Timestamp, Type, Value) ->
  {Timestamp, Type, Value}.

-spec measurementEquals(measurement(), measurement()) ->
  boolean().
measurementEquals({Ts, T, _}, {Ts, T, _}) ->
  true;
measurementEquals({_, _, _}, {_, _, _}) ->
  false.

-spec mean(list(float())) ->
  float() | not_available.
mean([]) ->
  not_available;
mean(List) when is_list(List) ->
  lists:sum(List) / length(List).

