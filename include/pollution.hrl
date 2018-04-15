%%%-------------------------------------------------------------------
%%% @author Kasper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2018 19:46
%%%-------------------------------------------------------------------
-author("Kasper").

-ifndef(POLLUTION_HRL).
-define(POLLUTION_HRL, 1).


-type station_name() :: string().
-type coords() :: {float(), float()}.
-type timestamp() :: calendar:datetime().
-type m_type() :: string().
-type measurement() :: {timestamp(), m_type(), float()}.
-type identifier() :: {name, station_name()} | {coords, coords()}.


-record(station, {
  name :: station_name(),
  coordinates :: coords(),
  measurements = [] :: [measurement()]
}).
-type station() :: #station{}.

-record(monitor, {
  stations = #{} :: #{station_name() => #station{}},
  coords_to_station_name = #{} :: #{coords() => station_name()}
}).
-type monitor() :: #monitor{}.


-endif.
