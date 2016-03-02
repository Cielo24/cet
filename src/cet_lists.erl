%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2016 Cielo24, Inc.
%%% @doc Utility functions used with lists.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_lists).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([join/2]).


-spec join(list(), Sep :: term()) -> list().
join([Elem | Tail], Sep) ->
    join(Tail, Sep, [Elem]);
join(List, _Sep) ->
    List.

join([Elem | Tail], Sep, Acc) ->
    join(Tail, Sep, [Elem, Sep | Acc]);
join([], _Sep, Acc) ->
    lists:reverse(Acc).

