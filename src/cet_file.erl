%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2015 Cielo24, Inc.
%%% @doc Utility functions used in HTTP handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_file).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([open_temp/3]).


open_temp(FileStem, FileExt, Mode) ->
    Suffix = integer_to_binary(erlang:phash2(make_ref()), 36),
    Filename = iolist_to_binary([FileStem, $_, Suffix, FileExt]),
    case file:open(Filename, Mode) of
        {ok, IoDevice} ->
            {ok, {Filename, IoDevice}};
        {error, Reason} when Reason =:= eisdir; Reason =:= eexist ->
            %% If the filename is a directory or an existing file, then we retry.
            open_temp(FileStem, FileExt, Mode);
        {error, Reason} = Error ->
            lager:warning("Could not open temporary file '~s' for writing: ~p~n", [Filename, Reason]),
            Error
    end.

