%%%-------------------------------------------------------------------
%% @copyright (C) 2015 Cielo24 Inc.
%% @doc Module for initializing mnesia database.
%% @end
%%%-------------------------------------------------------------------
-module(cet_mnesia).

-export([init/1, init/2]).

-type table_name() :: atom().
-type table_info() :: {access_mode, read_write | read_only}
                    | {attributes, [atom()]}
                    | {index, [integer()]}
                    | {load_order, [integer()]}
                    | {majority, boolean()}
                    | {record, atom()}
                    | {snmp, term()}
                    | {storage_properties, term()}
                    | {type, set | ordered_set | bag}
                    | {local_content, boolean()}.
-type table_spec() :: {table_name(), [table_info()]}.

-export_type([table_spec/0, table_name/0, table_info/0]).


%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the mnesia database with a given set of tables on the current node.
-spec init([table_spec()]) -> ok | no_return().
init(Tables) ->
    init(Tables, [node()]).

%% @doc Starts the mnesia database with a given set of tables on the given set
%%      of nodes.
-spec init([table_spec()], [node()]) -> ok | no_return().
init(Tables, Nodes) ->
    lager:info("Initializing mnesia database in ~s", [mnesia_dir()]),
    StorageType =
        case Nodes of
            %% There's no need to create an on-disk schema for Mnesia when there
            %% isn't a valid node name. This normally happens when running tests
            %% with common_test.
            [nonode@nohost] -> ram_copies;
            _               -> disc_copies
        end,
    ok = ensure_mnesia_started(Nodes),
    ok = ensure_mnesia_dir(),
    ok = ensure_schema_created(Nodes, StorageType),
    ok = ensure_tables_created(Tables, Nodes, StorageType).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_mnesia_started([node()]) -> ok | {error, Reason :: term()}.
ensure_mnesia_started(Nodes) ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        no ->
            start_mnesia(Nodes);
        starting ->
            lager:info("Waiting 500 ms for mnesia to start~n"),
            timer:sleep(500),
            ensure_mnesia_started(Nodes);
        stopping ->
            {error, mnesia_stopping}
    end.

-spec start_mnesia([node()]) -> ok | {error, any()}.
start_mnesia(_Nodes) ->
    % TODO start on all nodes
    mnesia:start().

-spec ensure_mnesia_dir() -> ok | {error, cannot_create_mnesia_dir}.
ensure_mnesia_dir() ->
    MnesiaDir = mnesia_dir() ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            lager:error("Cannot create mnesia directory '~s': ~p",
                        [MnesiaDir, mnesia:error_description(Reason)]),
            {error, cannot_create_mnesia_dir};
        ok ->
            ok
    end.

-spec ensure_schema_created([node()] | node(), ram_copies | disc_copies) -> ok | {error, any()}.
ensure_schema_created(Nodes, StorageType) when is_list(Nodes) ->
    lists:foreach(fun(Node) -> ok = ensure_schema_created(Node, StorageType) end, Nodes);
ensure_schema_created(Node, StorageType) ->
    %% This is a "hackish" way of making sure that a Mnesia schema exists on
    %% disk without shutting down Mnesia.
    %% When Mnesia is started without having called mnesia:create_schema/1 in
    %% advance, Mnesia will start with its 'schema' table in RAM and any attempt
    %% to create a table with a 'disc_copies' storage mode will fail. Also,
    %% mnesia:create_schema/1 will fail if called while Mnesia is up.
    %% To solve this problem, we use mnesia:change_table_copy_type/3 to make
    %% the default 'schema' disk-based.
    case mnesia:table_info(schema, storage_type) of
        StorageType ->
            lager:info("Mnesia schema is already stored as '~s'~n; no need to modify it", [StorageType]),
            ok;
        _ ->
            case mnesia:change_table_copy_type(schema, Node, StorageType) of
                {atomic, ok} ->
                    lager:info("Created Mnesia schema in node '~s'~n", [Node]),
                    ok;
                {aborted, {already_exists, schema, Node, StorageType}} ->
                    ok;
                {aborted, Reason} ->
                    lager:error("Failed to create Mnesia schema in node '~s': ~p~n", [Node, Reason]),
                    {error, Reason}
            end
    end.

-spec ensure_tables_created([table_spec()], [node()], disc_copies | ram_copies) -> ok.
ensure_tables_created(Tables, Nodes, StorageType) ->
    lists:foreach(fun(Table) -> ok = ensure_table_created(Table, Nodes, StorageType) end, Tables),
    ok.

-spec ensure_table_created(table_spec(), [node()], disc_copies | ram_copies) ->
                                  ok | {error, Reason :: term()}.
ensure_table_created({Name, Info} = _Table, Nodes, StorageType)
  when is_atom(Name), is_list(Info), is_list(Nodes) ->
    Attrs = proplists:get_value(attributes, Info),
    %% Make sure that the schema of the Mnesia table is up-to-date.
    try (length(Attrs) + 1 =:= mnesia:table_info(Name, arity) andalso
         (Attrs -- mnesia:table_info(Name, attributes)) =:= []) of
        true ->
            ok;
        false ->
            {error, {invalid_mnesia_table_schema, Name,
                     Attrs, mnesia:table_info(Name, attributes)}}
    catch
        exit:{aborted, {no_exists, Name, _}} ->
            create_table(Name, Info, Nodes, StorageType)
    end.

-spec create_table(table_name(), [table_info()], [node()], disc_copies | ram_copies) ->
                          ok | {error, Reason :: term()}.
create_table(Name, Info0, Nodes, StorageType) ->
    lager:info("Creating table '~s' with storage type '~s' on nodes ~p",
               [Name, StorageType, Nodes]),
    Info = lists:keystore(StorageType, 1, Info0, {StorageType, Nodes}),
    case mnesia:create_table(Name, Info) of
        {atomic, ok}      -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

-spec mnesia_dir() -> string().
mnesia_dir() ->
    mnesia:system_info(directory).
