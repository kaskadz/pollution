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
  0.

-spec addValue(identifier(), timestamp(), m_type(), float(), monitor()) ->
  monitor() | {error, exists}.
addValue(Identifier, Timestamp, Type, Value, Monitor) ->
  0.

-spec removeValue(identifier(), timestamp(), m_type(), monitor()) ->
  monitor().
removeValue(Identifier, Timestamp, Type, Monitor) ->
  0.

-spec getOneValue(identifier(), m_type(), timestamp(), monitor()) ->
  measurement() | {error, not_found}.
getOneValue(Identifier, Type, Timestamp, Monitor) ->
  0.

-spec getStationMean(identifier(), m_type(), monitor()) ->
  float() | not_available.
getStationMean(Identifier, Type, Monitor) ->
  0.

-spec getDailyMean(timestamp(), m_type(), monitor()) ->
  float() | not_available.
getDailyMean(Timestamp, Type, Monitor) ->
  0.

-spec getCorrelation(m_type(), m_type(), monitor()) ->
  float() | not_available.
getCorrelation(Type1, Type2, Monitor) ->
  0.

%% ===================
%% Auxiliary functions
%% ===================

%%Nie powinno być możliwe:
%%
%%dodanie dwóch stacji pomiarowych o tej samej nazwie lub tych samych współrzędnych;
%%dodanie dwóch pomiarów o tych samych:
%%współrzędnych,
%%dacie i godzinie,
%%typie (PM10, PM2.5, temperatura, …);
%%dodanie pomiaru do nieistniejącej stacji.

%%addValue/5 - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
%%removeValue/4 - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
%%getOneValue/4 - zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
%%getStationMean/3 - zwraca średnią wartość parametru danego typu z zadanej stacji;
%%getDailyMean/3 - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;