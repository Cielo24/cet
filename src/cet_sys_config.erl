%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2015 Cielo24, Inc.
%%% @doc Functions that render and load a sys.config file into the system.
%%%      Useful for testing projects with Erlang releases that use sys.config
%%%      files with relx/rebar overlay variables.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_sys_config).

-export([set_env/1]).
-export([render/2]).

-export_type([sys_config/0]).

-type sys_config() :: [{App :: atom(), [{Name :: atom(), Value :: term()}]}].


-spec set_env(sys_config()) -> ok | no_return().
set_env([{App, Env} | Tail]) ->
    application:load(App),
    ok = set_env(App, Env),
    set_env(Tail);
set_env([]) ->
    ok.

set_env(App, Env) ->
    lists:foreach(
      fun ({Name, Value}) ->
              ok = application:set_env(App, Name, Value, [{persistent, true}])
      end, Env).


-spec render(SysConfigFile :: file:name_all(), OverlayVarsFile :: file:name_all()) -> sys_config() | no_return().
render(SysConfigFile, OverlayVarsFile) ->
    {ok, SysConfigTemplate} = file:read_file(SysConfigFile),
    {ok, OverlayVars} = file:consult(OverlayVarsFile),
    %% Render the sys.config template using the provided overlay variables.
    SysConfig = bbmustache:render(SysConfigTemplate, OverlayVars, [{key_type, atom}]),
    eval(binary_to_list(SysConfig), []).


eval(Str, Bindings) ->
    %% Scan the code into tokens.
    {ok, Tokens, _} = erl_scan:string(Str),
    %% Parse the tokens into the abstract form.
    {ok, Form} = erl_parse:parse_exprs(Tokens),
    %% Evaluate the abstract form with the bindings to get the expressions from
    %% the original string.
    {value, Exprs, _} = erl_eval:exprs(Form, Bindings),
    Exprs.
