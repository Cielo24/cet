%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2015 Juan Jose Comellas
%%% @doc Simplified interface to send emails using gen_smtp.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_smtp).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-compile([{parse_transform, lager_transform}]).

-export([send_email/1, send_email/2]).
-export([smtp_options/2, smtp_option_value/2, smtp_option_value/3]).
-export([rfc5322_timestamp/1, pretty_timestamp/1, pretty_timestamp/2]).

-export_type([email/0]).

-define(APP,                              cet).

-define(AUTH,                             if_available).
-define(ENABLE_SSL,                       false).
-define(PORT,                             25).
-define(RELAY,                            "localhost").
-define(SENDER,                           "no-reply@cielo24.com").
-define(RETRIES,                          2).
-define(TLS,                              if_available).

-define(HDR_BCC,                          "Bcc").
-define(HDR_CC,                           "Cc").
-define(HDR_CONTENT_DISPOSITION,          "Content-Disposition").
-define(HDR_CONTENT_ID,                   "Content-ID").
-define(HDR_CONTENT_TRANSFER_ENCODING,    "Content-Transfer-Encoding").
-define(HDR_FROM,                         "From").
-define(HDR_SENDER,                       "Sender").
-define(HDR_SUBJECT,                      "Subject").
-define(HDR_REPLY_TO,                     "Reply-To").
-define(HDR_TO,                           "To").
-define(HDR_X_ATTACHMENT_ID,              "X-Attachment-Id").

-define(TYPE_APPLICATION,                 "application").
-define(TYPE_MULTIPART,                   "multipart").
-define(TYPE_TEXT,                        "text").

-define(SUBTYPE_ALTERNATIVE,              "alternative").
-define(SUBTYPE_HTML,                     "html").
-define(SUBTYPE_MIXED,                    "mixed").
-define(SUBTYPE_OCTET_STREAM,             "octet-stream").
-define(SUBTYPE_PLAIN,                    "plain").
-define(SUBTYPE_RELATED,                  "related").

-type name()                :: binary().
-type simple_address()      :: binary().
-type full_address_item()   :: {name, name()} | {email, simple_address()}.
-type full_address()        :: [full_address_item()] | {name(), simple_address()}.
-type address()             :: simple_address() | full_address().
-type subject()             :: binary().
-type attachment_item()     :: {disposition, attachment | inline}
                             | {content_type, {MediaType :: binary(), MediaSubtype :: binary()}}
                             | {content, iolist()}
                             | {filename, file:name()}
                             | {modification_date, calendar:datetime()}.
-type attachment()          :: [attachment_item()].
-type email_item()          :: {from, address()}
                             | {to, [address()]}
                             | {cc, [address()]}
                             | {bcc, [address()]}
                             | {reply_to, address()}
                             | {sender, address()}
                             | {subject, subject()}
                             | {text, binary()}
                             | {html, binary()}
                             | {attachments, [attachment()]}.
-type email()               :: [email_item()].


-spec send_email(email()) -> ok | {error, Reason :: term()}.
send_email(Email) ->
    send_email(Email, []).

-spec send_email(email(), Options :: proplists:proplist()) -> ok | {error, Reason :: term()}.
send_email(Email, Opts) ->
    Options = smtp_options(Opts),
    Sender = smtp_option_value(smtp_sender, Options, ?SENDER),
    Destinations = email_destinations(Email),
    Body = email_body(Email),
    lager:debug("Sending email from ~p to ~p with options: ~p~n",
                [Sender, Destinations, Options]),
    Msg = mimemail:encode(Body),
    Result = gen_smtp_client:send_blocking({Sender, Destinations, Msg}, Options),
    lager:debug("Email send finished with result: ~s~n", [Result]),
    case Result of
        <<"Message accepted\r\n">> ->
            email_ok(Result);
        <<"2", _/binary>> ->
            email_ok(Result);
        <<"Ok: queued as", _/binary>> ->
            email_ok(Result);
        <<"4", _/binary>> ->
            email_error(Result);
        <<"5", _/binary>> ->
            email_error(Result);
        _ ->
            email_error(Result)
    end.


%% smtp_options() ->
%%     smtp_options([]).

smtp_options(Options) ->
    smtp_options([{smtp_relay,      relay,     ?RELAY},
                  {smtp_port,       port,      ?PORT},
                  {smtp_hostname,   hostname},
                  {enable_smtp_ssl, ssl,       ?ENABLE_SSL},
                  {smtp_tls,        tls,       ?TLS},
                  {smtp_auth,       auth,      ?AUTH},
                  {smtp_username,   username},
                  {smtp_password,   password},
                  {smtp_retries,    retries,   ?RETRIES}], Options).

smtp_options(ConfigKeys, Options) ->
    smtp_options(ConfigKeys, Options, []).

smtp_options([{ConfigKey, SmtpKey, Default} | Tail], Options, Acc) ->
    NewAcc = [{SmtpKey, smtp_option_value(ConfigKey, Options, Default)} | Acc],
    smtp_options(Tail, Options, NewAcc);
smtp_options([{ConfigKey, SmtpKey} | Tail], Options, Acc) ->
    NewAcc = case smtp_option_value(ConfigKey, Options) of
                 undefined -> Acc;
                 Value     -> [{SmtpKey, Value} | Acc]
             end,
    smtp_options(Tail, Options, NewAcc);
smtp_options([], _Options, Acc) ->
    lists:reverse(Acc).

smtp_option_value(ConfigKey, Options) ->
    smtp_option_value(ConfigKey, Options, undefined).

smtp_option_value(ConfigKey, Options, Default) ->
    case proplists:get_value(ConfigKey, Options) of
        undefined -> application:get_env(?APP, ConfigKey, Default);
        Value     -> Value
    end.


email_destinations(EmailDoc) ->
    email_destinations(EmailDoc, [to, cc, bcc], []).

email_destinations(EmailDoc, [Key | Tail], Acc) ->
    NewAcc = case proplists:get_value(Key, EmailDoc) of
                 Destination when is_list(Destination) -> Destination ++ Acc;
                 undefined                             -> Acc
             end,
    email_destinations(EmailDoc, Tail, NewAcc);
email_destinations(_EmailDoc, [], Acc) ->
    format_addresses(lists:reverse(Acc)).


email_body(EmailDoc) ->
    Headers = email_headers(EmailDoc),
    case attachment_parts(EmailDoc) of
        [] ->
            case text_part(EmailDoc, Headers) of
                undefined ->
                    {<<?TYPE_TEXT>>, <<?SUBTYPE_PLAIN>>, Headers, [], <<>>};
                TextPart ->
                    TextPart
            end;
        [_ | _] = AttachmentParts ->
            Parts = case text_part(EmailDoc) of
                        undefined -> AttachmentParts;
                        TextPart  -> [TextPart | AttachmentParts]
                    end,
            {<<?TYPE_MULTIPART>>, <<?SUBTYPE_MIXED>>, Headers, [], Parts}
    end.


email_headers(EmailDoc) ->
    email_headers(EmailDoc, []).

email_headers([Head | Tail], Acc) ->
    NewAcc = case email_header(Head) of
                 {_Name, _Value} = Header -> [Header | Acc];
                 undefined                -> Acc
             end,
    email_headers(Tail, NewAcc);
email_headers([], Acc) ->
    lists:reverse(Acc).

email_header({from, Value})       -> {<<?HDR_FROM>>,     format_address(Value)};
email_header({to, Value})         -> {<<?HDR_TO>>,       format_address_line(Value)};
email_header({cc, Value})         -> {<<?HDR_CC>>,       format_address_line(Value)};
email_header({bcc, Value})        -> {<<?HDR_BCC>>,      format_address_line(Value)};
email_header({reply_to, Value})   -> {<<?HDR_REPLY_TO>>, format_address(Value)};
email_header({sender, Value})     -> {<<?HDR_SENDER>>,   format_address(Value)};
email_header({subject, Value})    -> {<<?HDR_SUBJECT>>,  Value};
email_header(_Tuple)              -> undefined.


text_part(EmailDoc) ->
    text_part(EmailDoc, []).

text_part(EmailDoc, Headers) ->
    Text = proplists:get_value(text, EmailDoc),
    Html = proplists:get_value(html, EmailDoc),
    %% General parameters used by all text parts
    Parameters = text_parameters(),
    if
        Text =/= undefined, Html =:= undefined ->
            {<<?TYPE_TEXT>>, <<?SUBTYPE_PLAIN>>, Headers, Parameters, Text};
        Text =:= undefined, Html =/= undefined ->
            {<<?TYPE_TEXT>>, <<?SUBTYPE_HTML>>, Headers, Parameters, Html};
        Text =/= undefined, Html =/= undefined ->
            Subparts = [{<<?TYPE_TEXT>>, <<?SUBTYPE_PLAIN>>, [], Parameters, Text},
                        {<<?TYPE_TEXT>>, <<?SUBTYPE_HTML>>, [], Parameters, Html}],
            %% Nested alternative multipart section
            {<<?TYPE_MULTIPART>>, <<?SUBTYPE_ALTERNATIVE>>, Headers, [], Subparts};
        true ->
            undefined
    end.

text_parameters() ->
    [{<<"diposition">>, <<"inline">>},
     {<<"content-type-params">>, [{<<"charset">>, <<"UTF-8">>}]}].


attachment_parts(EmailDoc) ->
    attachment_parts(proplists:get_value(attachments, EmailDoc, []), []).

attachment_parts([Attachment | Tail], Acc) ->
    attachment_parts(Tail, [attachment_part(Attachment) | Acc]);
attachment_parts([], Acc) ->
    lists:reverse(Acc).

attachment_part(Attachment) ->
    {Type, Subtype} = proplists:get_value(content_type, Attachment,
                                        {<<?TYPE_APPLICATION>>, <<?SUBTYPE_OCTET_STREAM>>}),
    Name = attachment_name(Attachment),
    %% Create a unique ID for the attachment.
    AttachmentId = integer_to_binary(erlang:crc32([Name, rand_bytes(2)]), 16),
    Headers = [{<<?HDR_CONTENT_TRANSFER_ENCODING>>, <<"base64">>},
               {<<?HDR_X_ATTACHMENT_ID>>, AttachmentId}],
    Parameters = [{<<"content-type-params">>, content_type_params(Attachment)},
                  {<<"disposition">>, attachment_disposition(Attachment)},
                  {<<"disposition-params">>, disposition_params(Attachment)}],
    Content = case proplists:get_value(content, Attachment) of
                  undefined ->
                      Filename = proplists:get_value(filename, Attachment, Name),
                      {ok, Data} = file:read_file(Filename),
                      Data;
                  Data ->
                      Data
              end,
    {Type, Subtype, Headers, Parameters, Content}.


attachment_name(Attachment) ->
    case proplists:get_value(name, Attachment) of
        Bin when is_binary(Bin) ->
            Bin;
        undefined ->
            RandomId = bstr:hexencode(rand_bytes(4)),
            <<"att", RandomId/binary>>
    end.

attachment_disposition(Attachment) ->
    case proplists:get_value(disposition, Attachment) of
        attachment              -> <<"attachment">>;
        inline                  -> <<"inline">>;
        Bin when is_binary(Bin) -> Bin
    end.

content_type_params(Attachment) ->
    case proplists:get_value(name, Attachment) of
        Name when is_binary(Name) -> [{<<"name">>, Name}];
        undefined                 -> []
    end.

disposition_params(Attachment) ->
    Filename = proplists:get_value(filename, Attachment),
    FilenameParams = if
                         is_binary(Filename) ->
                             [{<<"filename">>, Filename}];
                         true ->
                             []
                     end,
    ModificationDate = proplists:get_value(modification_date, Attachment),
    DateParams = if
                     is_integer(ModificationDate); is_tuple(ModificationDate) ->
                         [{<<"modification-date">>, rfc5322_timestamp(ModificationDate)}];
                     ModificationDate =:= undefined, Filename =/= undefined ->
                         %% If a filename was provided but there was no "modification-date"
                         %% set, retrieve if from the filesystem.
                         case filelib:last_modified(Filename) of
                             Datetime when is_tuple(Datetime) ->
                                 [{<<"modification-date">>, rfc5322_timestamp(Datetime)}];
                             0 ->
                                 []
                         end;
                     true ->
                         []
                 end,
    FilenameParams ++ DateParams.


format_addresses(Addresses) ->
    format_addresses(Addresses, []).

format_addresses([undefined | Tail], Acc) ->
    format_addresses(Tail, Acc);
format_addresses([Address | Tail], Acc) ->
    format_addresses(Tail, [format_address(Address) | Acc]);
format_addresses([], Acc) ->
    lists:reverse(Acc).


format_address_line([undefined | Tail]) ->
    format_address_line(Tail);
format_address_line([Address | Tail]) ->
    format_address_line(Tail, [format_address(Address)]);
format_address_line([]) ->
    <<>>.

format_address_line([undefined | Tail], Acc) ->
    format_address_line(Tail, Acc);
format_address_line([Address | Tail], Acc) ->
    format_address_line(Tail, [format_address(Address), $, | Acc]);
format_address_line([], Acc) ->
    iolist_to_binary(lists:reverse(Acc)).


format_address(undefined) ->
    <<>>;
format_address(Value) when is_binary(Value) ->
    Value;
format_address({Name, Address}) when is_binary(Name), is_binary(Address) ->
    %% TODO: should we encode/escape the name?
    format_address(Name, Address);
format_address([_ | _] = Value) ->
    Name = proplists:get_value(name, Value),
    Address = proplists:get_value(email, Value),
    format_address(Name, Address).

format_address(Name, Address) when is_binary(Name), is_binary(Address) ->
    EscapedName = case re:run(Name, <<"\"">>) of
                      {match, _} ->
                          re:replace(Name, <<"\"">>, <<"\\\\\"">>, [global]);
                      nomatch ->
                          Name
                  end,
    iolist_to_binary([$", EscapedName, <<"\" <">>, Address, $>]);
format_address(undefined, Address) when is_binary(Address) ->
    Address.


-define(DAYS, [<<"Mon">>, <<"Tue">>, <<"Wed">>, <<"Thu">>,
               <<"Fri">>, <<"Sat">>, <<"Sun">>]).
-define(MONTHS, [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                 <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).

%% @doc Generate a RFC 5322 timestamp based on the time passed as seconds since
%% the Unix epoch (Jan 1, 1970, 00:00:00).
rfc5322_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    %% {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    NDay = calendar:day_of_the_week(Year, Month, Day),
    DoW = lists:nth(NDay, ?DAYS),
    MoY = lists:nth(Month, ?MONTHS),
    iolist_to_binary(io_lib:format("~s, ~b ~s ~b ~2..0b:~2..0b:~2..0b ~s",
                                   [DoW, Day, MoY, Year, Hour, Minute, Second, zone()])).

pretty_timestamp(Datetime) ->
    pretty_timestamp(Datetime, zone()).

pretty_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}, Zone) ->
    NDay = calendar:day_of_the_week(Year, Month, Day),
    DoW = lists:nth(NDay, ?DAYS),
    MoY = lists:nth(Month, ?MONTHS),
    {Hour12, AmPm} = if
                         Hour =< 12 -> {Hour, <<"AM">>};
                         true       -> {Hour rem 12, <<"PM">>}
                     end,
    iolist_to_binary(io_lib:format("~s, ~s ~b, ~b at ~2..0b:~2..0b:~2..0b ~s ~s",
                                   [DoW, MoY, Day, Year, Hour12, Minute, Second, AmPm, Zone])).


%% @doc Calculate the current timezone and format it like -0400. Borrowed from YAWS.
zone() ->
        Time = erlang:universaltime(),
        LocalTime = calendar:universal_time_to_local_time(Time),
        DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
        calendar:datetime_to_gregorian_seconds(Time),
        zone((DiffSecs / 3600) * 100).

%% Ugly reformatting code to get times like +0000 and -1300
zone(Val) when Val < 0 ->
        io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
        io_lib:format("+~4..0w", [trunc(abs(Val))]).


email_ok(_Result) ->
    lager:debug("Email sent: ~p", [_Result]),
    ok.

%% email_error(Msg, Code) ->
%%     lager:warning("Email send failed: ~p", [Msg]),
%%     {error, {Code, Msg}}.

email_error(Result) ->
    lager:warning("Email send failed: ~p", [Result]),
    {error, {email_send_failed, Result}}.


rand_bytes(Length) ->
    try crypto:strong_rand_bytes(Length) of
        Bytes ->
            Bytes
    catch
        _:low_entropy ->
            crypto:rand_bytes(Length)
    end.
