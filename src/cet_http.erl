%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2015 Cielo24, Inc.
%%% @doc Utility functions used in HTTP handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_http).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-compile([{parse_transform, lager_transform}]).

-export([common_log/4]).
-export([get_body/1, get_body/2]).
-export([get_body_strict/2]).
-export([get_part_body/1, get_part_body/2]).
-export([peer_ip/1]).
-export([reply_halt_status/3]).
-export([reply_html_text/2]).
-export([reply_json_term/2]).
-export([reply_json_text/2]).
-export([status_reason/1]).
-export([verify_token/3]).
-export([write_body/4]).
-export([write_part_body/4]).

-define(HDR_CONTENT_TYPE, "content-type").
-define(HDR_X_FORWARDED_FOR, "x-forwarded-for").

-define(MIME_JSON, "application/json").
-define(MIME_HTML, "text/html").

-define(MAX_BODY_LENGTH, 10485760).

-type req()               :: cowboy_req:req().
-type write_body_option() :: {max_length, non_neg_integer()}
                           | {compress, boolean()}.


-spec common_log(cowboy:status(), cowboy:headers(), iodata(), req()) -> req().
common_log(Status, _Headers, Body, Req0) ->
    %% 127.0.0.1 - - [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326
    [Method, Version, Path, Qs0] = cowboy_req:get([method, version, path, qs], Req0),
    {Addr, Req1} = case cowboy_req:peer(Req0) of
                       {{IpAddr, _Port}, Req2} -> {inet:ntoa(IpAddr), Req2};
                       {undefined, Req2}       -> {<<"<undefined>">>, Req2}
                   end,
    Datetime = calendar:now_to_universal_time(os:timestamp()),
    Iso8601Date = cet_time:datetime_to_iso8601(Datetime),
    Qs = case Qs0 of
             <<>> -> Qs0;
             _    -> [$?, Qs0]
         end,
    Size = iolist_size(Body),
    case cowboy_req:header(<<?HDR_X_FORWARDED_FOR>>, Req1) of
        {undefined, Req} ->
            lager:info("~s - - [~s] \"~s ~s~s ~s\" ~b ~b",
                       [Addr, Iso8601Date, Method, Path, Qs, Version, Status, Size]),
            Req;
        {ForwardedAddrs0, Req} ->
            %% The X-Forwarded-For header value looks like this: "client, proxy1, proxy2".
            ForwardedAddrs = [[bstr:lstrip(ForwardedAddr, $\s), <<"->">>] ||
                                 ForwardedAddr <- binary:split(ForwardedAddrs0, <<",">>, [global, trim])],
            lager:info("~s~s - - [~s] \"~s ~s~s ~s\" ~b ~b",
                       [iolist_to_binary(ForwardedAddrs), Addr, Iso8601Date,
                        Method, Path, Qs, Version, Status, Size]),
            Req
    end.


-spec get_body(req()) -> {ok, Body :: binary(), req()} | {error, Reason :: term()}.
get_body(Req) ->
    get_body_relaxed(Req, ?MAX_BODY_LENGTH, <<>>).


-spec get_body(req(), Length :: non_neg_integer()) -> {ok, Body :: binary(), req()} |
                                                      {error, Reason :: term()}.
get_body(Req, Length) when is_integer(Length) ->
    get_body_relaxed(Req, Length, <<>>).


get_body_relaxed(Req0, Length, Buffer) when is_integer(Length) ->
    case cowboy_req:body(Req0, [{length, Length}]) of
        {Status, Data, Req} ->
            if
                byte_size(Buffer) + byte_size(Data) =< Length ->
                    %% To avoid allocating memory unnecessarily, we don't
                    %% concatenate the received data to the existing buffer
                    %% until we're sure that the resulting body size is valid.
                    Body = <<Buffer/binary, Data/binary>>,
                    case Status of
                        ok   -> {ok, Body, Req};
                        more -> get_body_relaxed(Req, Length, Body)
                    end;
                true ->
                    {error, badlength}
            end;
        {error, Reason} = Error ->
            lager:info("Could not read HTTP request body from IP ~s: ~p~n",
                       [peer_ip(Req0), Reason]),
            Error
    end.


-spec get_body_strict(req(), Length :: non_neg_integer()) -> {ok, Body :: binary(), req()} |
                                                             {error, Reason :: term()}.
get_body_strict(Req, Length) when is_integer(Length) ->
    get_body_strict(Req, Length, <<>>).

get_body_strict(Req0, Length, Buffer) when is_integer(Length) ->
    case cowboy_req:body(Req0, [{length, Length}]) of
        {Status, Data, Req} ->
            %% To avoid allocating memory unnecessarily, we don't
            %% concatenate the received data to the existing buffer
            %% until we're sure that the resulting body size is valid.
            BodySize = byte_size(Buffer) + byte_size(Data),
            case Status of
                ok when BodySize =:= Length ->
                    Body = <<Buffer/binary, Data/binary>>,
                    {ok, Body, Req};
                more when BodySize =< Length ->
                    Body = <<Buffer/binary, Data/binary>>,
                    get_body_strict(Req, Length, Body);
                _ ->
                    {error, badlength}
            end;
        {error, Reason} = Error ->
            lager:info("Could not read HTTP request body from IP ~s: ~p~n",
                       [peer_ip(Req0), Reason]),
            Error
    end.


-spec get_part_body(req()) -> {ok, Body :: binary(), req()} | {error, Reason :: term(), req()}.
get_part_body(Req) ->
    get_part_body_relaxed(Req, ?MAX_BODY_LENGTH, <<>>).

-spec get_part_body(req(), Length :: non_neg_integer()) -> {ok, Body :: binary(), req()} |
                                                           {error, Reason :: term(), req()}.
get_part_body(Req, MaxLength) when is_integer(MaxLength) ->
    get_part_body_relaxed(Req, MaxLength, <<>>).

get_part_body_relaxed(Req0, MaxLength, Buffer) ->
    {Status, Chunk, Req} = cowboy_req:part_body(Req0),
    NewLength = byte_size(Buffer) + byte_size(Chunk),
    case Status of
        ok when NewLength =< MaxLength ->
            {ok, <<Buffer/binary, Chunk/binary>>, Req};
        ok ->
            lager:warning("Body in HTTP part is bigger than ~w bytes~n", [MaxLength]),
            {error, file_too_big, Req};
        more ->
            get_part_body_relaxed(Req, MaxLength, <<Buffer/binary, Chunk/binary>>)
    end.


-spec peer_ip(req()) -> Address :: binary().
peer_ip(Req0) ->
    case cowboy_req:header(<<?HDR_X_FORWARDED_FOR>>, Req0) of
        {undefined, Req1} ->
            %% If the request came directly from the PBX, get the originating
            %% IP from the request.
            {{IpAddress, _Port}, _Req} = cowboy_req:peer(Req1),
            list_to_binary(inet:ntoa(IpAddress));
        {Clients, _Req} ->
            %% If the request came from a proxy, get the originating IP from
            %% the HTTP "x-forwarded-for" header
            [IpAddress | _Tail] = binary:split(Clients, <<",">>),
            IpAddress
    end.


-spec reply_halt_status(StatusCode :: non_neg_integer(), req(), State :: term()) ->
                               {halt, req(), State :: term()}.
reply_halt_status(StatusCode, Req0, State) ->
    {ok, Req} = cowboy_req:reply(StatusCode, Req0),
    {halt, Req, State}.


-spec reply_html_text(iodata(), req()) -> req().
reply_html_text(HtmlText, Req0) ->
    Req = cowboy_req:set_resp_header(<<?HDR_CONTENT_TYPE>>, <<?MIME_HTML>>, Req0),
    cowboy_req:set_resp_body(HtmlText, Req).


-spec reply_json_term(jsx:json_term(), req()) -> req().
reply_json_term(JsonTerm, Req) ->
    reply_json_text(jsx:encode(JsonTerm), Req).


-spec reply_json_text(jsx:json_term(), req()) -> req().
reply_json_text(JsonText, Req0) ->
    Req = cowboy_req:set_resp_header(<<?HDR_CONTENT_TYPE>>, <<?MIME_JSON>>, Req0),
    cowboy_req:set_resp_body(JsonText, Req).


-spec status_reason(non_neg_integer()) -> atom().
status_reason(100) -> continue;
status_reason(101) -> switching_protocols;
status_reason(102) -> processing;
status_reason(200) -> ok;
status_reason(201) -> created;
status_reason(202) -> accepted;
status_reason(203) -> non_authoritative_information;
status_reason(204) -> no_content;
status_reason(205) -> reset_content;
status_reason(206) -> partial_content;
status_reason(207) -> multi_status;
status_reason(300) -> multiple_choices;
status_reason(301) -> moved_permanently;
status_reason(302) -> found;
status_reason(303) -> see_other;
status_reason(304) -> not_modified;
status_reason(305) -> use_proxy;
status_reason(306) -> unused;
status_reason(307) -> temporary_redirect;
status_reason(400) -> bad_request;
status_reason(401) -> unauthorized;
status_reason(402) -> payment_required;
status_reason(403) -> forbidden;
status_reason(404) -> not_found;
status_reason(405) -> method_not_allowed;
status_reason(406) -> not_acceptable;
status_reason(407) -> proxy_authentication_required;
status_reason(408) -> request_timeout;
status_reason(409) -> conflict;
status_reason(410) -> gone;
status_reason(411) -> length_required;
status_reason(412) -> precondition_failed;
status_reason(413) -> request_entity_too_large;
status_reason(414) -> request_uri_too_long;
status_reason(415) -> unsupported_media_type;
status_reason(416) -> requested_range_not_satisfiable;
status_reason(417) -> expectation_failed;
status_reason(422) -> unprocessable_entity;
status_reason(423) -> locked;
status_reason(424) -> failed_dependency;
status_reason(500) -> internal_server_error;
status_reason(501) -> not_implemented;
status_reason(502) -> bad_gateway;
status_reason(503) -> service_unavailable;
status_reason(504) -> gateway_timeout;
status_reason(505) -> http_version_not_supported;
status_reason(507) -> insufficient_storage;
status_reason(X) when is_binary(X) -> status_reason(binary_to_integer(X));
status_reason(_)   -> unknown_status_code.


-spec verify_token(HeaderName :: binary(), Secret :: binary(), req()) -> {boolean(), req()}.
verify_token(HeaderName, Secret, Req0) ->
    {Method, Req1} = cowboy_req:method(Req0),
    case cowboy_req:header(HeaderName, Req1) of
        {Token, Req} when is_binary(Token) ->
            case cet_token:verify(Token, Secret) of
                ok ->
                    {true, Req};
                {error, Reason} ->
                    lager:notice("Invalid token '~s' in ~s request from IP ~s: ~p~n",
                                 [Token, Method, peer_ip(Req), Reason]),
                    {false, Req}
            end;
        {undefined, Req} ->
            lager:notice("Missing '~s' header in ~s request from IP ~s~n",
                         [HeaderName, Method, peer_ip(Req)]),
            {false, Req}
    end.


-spec write_body(FileStem :: binary(), FileExt :: binary(), req(), [write_body_option()]) ->
                        {ok, Filename :: binary(), req()} |
                        {error, Reason :: term(), req()}.
write_body(FileStem, FileExt, Req, Options) ->
    write_body(fun (Req1) -> cowboy_req:body(Req1) end, FileStem, FileExt, Req, Options).


-spec write_part_body(FileStem :: binary(), FileExt :: binary(), req(), [write_body_option()]) ->
                             {ok, Filename :: binary(), req()} |
                             {error, Reason :: term(), req()}.
write_part_body(FileStem, FileExt, Req, Options) ->
    write_body(fun (Req1) -> cowboy_req:part_body(Req1) end, FileStem, FileExt, Req, Options).


write_body(ReadBody, FileStem, FileExt, Req, Options)
  when is_binary(FileStem), is_binary(FileExt), is_list(Options) ->
    case cet_file:open_temp(FileStem, FileExt, [write, exclusive, raw, binary, delayed_write]) of
        {ok, {Filename, IoDevice}} ->
            try
                %% Warning: these functions will not be tail-recursive (given
                %% that they are inside a try block) and might blow up the
                %% stack if the file being downloaded is too big.
                MaxLength = proplists:get_value(max_length, Options, ?MAX_BODY_LENGTH),
                case proplists:get_value(compress, Options, false) of
                    true ->
                        write_gzip_body(ReadBody, Filename, IoDevice, MaxLength, Req);
                    _ ->
                        write_raw_body(ReadBody, Filename, IoDevice, MaxLength, Req)
                end
            after
                file:close(IoDevice)
            end;
        {error, Reason} ->
            {error, Reason, Req}
    end.


-define(MAX_WBITS, 15).

write_gzip_body(ReadBody, Filename, IoDevice, MaxLength, Req0) ->
    ZStream = zlib:open(),
    try
        %% Emulate the initialization made by zlib:gzip/1.
        zlib:deflateInit(ZStream, default, deflated, 16 + ?MAX_WBITS, 8, default),
        case write_gzip_body_chunk(ReadBody, Filename, IoDevice, ZStream, 0, MaxLength, Req0) of
            {ok, _Filename, _Req} = Result ->
                zlib:deflateEnd(ZStream),
                Result;
            {error, _Reason, _Req} = Error ->
                %% On Unix operating systems a file can be deleted while open.
                file:delete(Filename),
                Error
        end
    after
        zlib:close(ZStream)
    end.

write_gzip_body_chunk(ReadBody, Filename, IoDevice, ZStream, Length, MaxLength, Req0) ->
    {Status, Chunk, Req} = ReadBody(Req0),
    NewLength = Length + byte_size(Chunk),
    if
        NewLength =< MaxLength ->
            GZippedChunk = zlib:deflate(ZStream, Chunk, deflate_flush(Status)),
            case file:write(IoDevice, GZippedChunk) of
                ok when Status =:= ok ->
                    {ok, Filename, Req};
                ok when Status =:= more ->
                    write_gzip_body_chunk(ReadBody, Filename, IoDevice, ZStream,
                                          NewLength, MaxLength, Req);
                {error, enospc} ->
                    lager:error("Not enough space on filesystem to write to file '~s'~n", [Filename]),
                    {error, enospc, Req};
                {error, Reason} ->
                    lager:warning("Could not write to file '~s': ~p~n", [Filename, Reason]),
                    {error, Reason, Req}
            end;
       true ->
            lager:warning("File '~s' is too big; tried to write more than ~w bytes to it~n",
                          [Filename, MaxLength]),
            {error, file_too_big, Req}
    end.

deflate_flush(more) -> none;
deflate_flush(ok)   -> finish.


write_raw_body(ReadBody, Filename, IoDevice, MaxLength, Req0) ->
    %% Warning: this function will not be tail-recursive (given
    %% that it is inside a try block) and might blow up the stack
    %% if the file being transferred is too big.
    case write_raw_body_chunk(ReadBody, Filename, IoDevice, 0, MaxLength, Req0) of
        {ok, _Filename, _Req} = Result ->
            Result;
        {error, _Reason, _Req} = Error ->
            %% On Unix operating systems a file can be deleted while open.
            file:delete(Filename),
            Error
    end.

write_raw_body_chunk(ReadBody, Filename, IoDevice, Length, MaxLength, Req0) ->
    {Status, Chunk, Req} = ReadBody(Req0),
    NewLength = Length + byte_size(Chunk),
    if
        NewLength =< MaxLength ->
            case file:write(IoDevice, Chunk) of
                ok when Status =:= ok ->
                    {ok, Filename, Req};
                ok when Status =:= more ->
                    write_raw_body_chunk(ReadBody, Filename, IoDevice, NewLength, MaxLength, Req);
                {error, enospc} ->
                    lager:error("Not enough space on filesystem to write to file '~s'~n", [Filename]),
                    {error, enospc, Req};
                {error, Reason} ->
                    lager:warning("Could not write to file '~s': ~p~n", [Filename, Reason]),
                    {error, Reason, Req}
            end;
       true ->
            lager:warning("File '~s' is too big; tried to write more than ~w bytes to it~n",
                          [Filename, MaxLength]),
            {error, file_too_big, Req}
    end.
