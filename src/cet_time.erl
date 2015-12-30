%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2015 Cielo24, Inc.
%%% @doc Date/time utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_time).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-compile([{parse_transform, lager_transform}]).

-export([datetime_to_iso8601/1]).


%% @doc Convert a date and time in the format returned by
%%      <code>calendar:universal_time/0</code> to a binary string in the
%%      ISO-8601 format (e.g. "2012-02-15T14:39:15Z"; "2012-02-15T14:39:15.671Z").
-spec datetime_to_iso8601(calendar:datetime()) -> binary().
datetime_to_iso8601({{_, _, _}, {_, _, _}} = Datetime) ->
    datetime_to_iso8601(Datetime, <<$Z>>).

-spec datetime_to_iso8601(calendar:datetime(), Suffix :: binary()) -> binary().
datetime_to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}, Suffix) ->
    ToBin = fun (I, Width) -> bstr:lpad(integer_to_binary(I), Width, $0) end,
    YYYY = ToBin(Year, 4),
    MM   = ToBin(Month, 2),
    DD   = ToBin(Day, 2),
    Hh   = ToBin(Hour, 2),
    Mm   = ToBin(Min, 2),
    Ss   = ToBin(Sec, 2),
    <<YYYY/binary, $-, MM/binary, $-, DD/binary, $T,
      Hh/binary, $:, Mm/binary, $:, Ss/binary, Suffix/binary>>.
