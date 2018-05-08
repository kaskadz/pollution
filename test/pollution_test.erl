%%%-------------------------------------------------------------------
%%% @author Kasper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2018 19:11
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Kasper").

-include("pollution.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(S_NAME1, "station1").
-define(S_NAME2, "Krakow2").
-define(S_COORDS1, {12.3, 45.67}).
-define(S_COORDS2, {-9.876, 54.32}).
-define(TIMESTAMP1, {{2018, 04, 16}, {6, 30, 15}}).
-define(TIMESTAMP2, {{2018, 03, 30}, {12, 34, 56}}).
-define(M_TYPE1, "PM10").
-define(M_TYPE2, "PM2.5").
-define(VALUE1, 2.1).
-define(VALUE2, 3.7).

-define(MEASUREMENT1, {?TIMESTAMP1, ?M_TYPE1, ?VALUE1}).
-define(ANY_MEASUREMENT, {_, _, _}).

simple_test() ->
  ?assert(true).

createMonitor_test() ->
  ?assertEqual(#monitor{}, pollution:createMonitor()).

addStation_test() ->
  M = pollution:createMonitor(),
  ?assertMatch(#monitor{}, pollution:addStation(?S_NAME1, ?S_COORDS1, M)).

addStation_duplication_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation(?S_NAME1, ?S_COORDS1, M1),

  [
    ?_assertMatch({error, _}, pollution:addStation(?S_NAME1, ?S_COORDS2, M2)),
    ?_assertMatch({error, _}, pollution:addStation(?S_NAME2, ?S_COORDS1, M2)),
    ?_assertMatch({error, _}, pollution:addStation(?S_NAME1, ?S_COORDS1, M2))
  ].

getStation_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation(?S_NAME1, ?S_COORDS1, M1),
  [
    ?_assertEqual(#station{name = ?S_NAME1, coordinates = ?S_COORDS1, measurements = []}, pollution:getStation({name, ?S_NAME1}, M2)),
    ?_assertEqual(#station{name = ?S_NAME1, coordinates = ?S_COORDS1, measurements = []}, pollution:getStation({coords, ?S_COORDS2}, M2))
  ].

addValue_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation(?S_NAME1, ?S_COORDS1, M1),
  MbyName = pollution:addValue({name, ?S_NAME1}, ?TIMESTAMP1, ?M_TYPE1, ?VALUE1, M2),
  MbyCoords = pollution:addValue({coords, ?S_COORDS1}, ?TIMESTAMP1, ?M_TYPE1, ?VALUE1, M2),
  [
    {
      "adding value by name",
      ?_assertMatch(#monitor{}, MbyName)
    },
    {
      "adding value by coords",
      ?_assertMatch(#monitor{}, MbyCoords)
    },
    {
      "adding value by name and by coords",
      ?_assertEqual(MbyCoords, MbyName)
    },
    {
      "finding value added by addValue",
      ?_assertEqual(?MEASUREMENT1, pollution:getOneValue({name, ?S_NAME1}, ?M_TYPE1, ?TIMESTAMP1, MbyName))
    },
    {
      "addValue allows adding duplicated measurements",
      ?_assertEqual({error, exists}, pollution:addValue({coords, ?S_COORDS1}, ?TIMESTAMP1, ?M_TYPE1, ?VALUE1, MbyCoords))
    },
    {
      "addValue allows adding measurements to non existing stations",
      ?_assertEqual({error, not_found}, pollution:addValue({name, ?S_COORDS2}, ?TIMESTAMP1, ?M_TYPE1, ?VALUE1, MbyCoords))
    }
  ].

%%removeValue_test() ->

