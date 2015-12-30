%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2015 Cielo24, Inc.
%%% @doc Functions that decode and encode API tokens.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_token).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([encode/2, encode/3, encode/4]).
-export([hash_secret/3]).
-export([decode/1]).
-export([verify/2, verify/4]).
-export([rand_bytes/1]).

-define(VERSION, "1").
-define(SALT_LENGTH, 20).


%% @doc Generate the API token used between Cielo24 and the Media Data Proxy
%%      Server. The format of the resulting `Token` is:
%%      <code>Version:Salt:Expiration:HashedSecret</code>
-spec encode(Secret :: binary(), Lifetime :: non_neg_integer()) -> Token :: binary().
encode(Secret, Lifetime) ->
    encode(Secret, rand_bytes(?SALT_LENGTH), Lifetime).

-spec encode(Secret :: binary(), Salt :: binary(), Lifetime :: non_neg_integer()) -> Token :: binary().
encode(Secret, Salt, Lifetime) ->
    encode(Secret, Salt, Lifetime, os:system_time(seconds)).

-spec encode(Secret :: binary(), Salt :: binary(), Lifetime :: non_neg_integer(),
             Epoch :: non_neg_integer()) -> Token :: binary().
encode(Secret, Salt, Lifetime, Epoch)
  when is_binary(Secret), is_binary(Salt), is_integer(Lifetime), is_integer(Epoch) ->
    Expiration = Epoch + Lifetime,
    Base64Salt = base64:encode(Salt),
    HexExpiration = bstr:lower(integer_to_binary(Expiration, 16)),
    Base64HashedSecret = base64:encode(hash_secret(Secret, Salt, Expiration)),
    <<?VERSION, $:, Base64Salt/binary, $:, HexExpiration/binary, $:, Base64HashedSecret/binary>>.


-spec hash_secret(Secret :: binary(), Salt :: binary(),
                  Expiration :: non_neg_integer()) -> Token :: binary().
hash_secret(Secret, Salt, Expiration) ->
    %% Hash the salt together with the secret: SHA1(Salt + Secret)
    SaltedSecret = crypto:hash(sha, <<Salt/binary, Secret/binary>>),
    %% Hash the least 32 significant bits (in network byte order) from the timestamp
    %% with the hashed salt and secret: SHA1(Timestamp + SHA1(Salt + Secret)).
    crypto:hash(sha, <<Expiration:32/unsigned-integer, SaltedSecret/binary>>).


%% @doc Decode a API <code>Token</code>.
-spec decode(Token :: binary()) ->
                    {ok, {Salt :: binary(), Epoch :: non_neg_integer(), HashedSecret :: binary()}} |
                    {error, Reason :: term()}.
decode(Token) ->
    case binary:split(Token, <<$:>>, [global]) of
        [Version, Salt, Expiration, HashedSecret] when Version =:= <<?VERSION>> ->
            {ok, {base64:decode(Salt), binary_to_integer(Expiration, 16), base64:decode(HashedSecret)}};
        _ ->
            {error, invalid_token_version}
    end.


%% @doc Check that an API <code>Token</code> has not expired and was encoded
%%      properly with the given <code>Secret</code>.
-spec verify(Token :: binary(), Secret :: binary()) -> ok | {error, Reason :: term()}.
verify(Token, Secret) ->
    case binary:split(Token, <<$:>>, [global]) of
        [Version, Salt, Expiration, HashedSecret] when Version =:= <<?VERSION>> ->
            verify(Secret, base64:decode(Salt), binary_to_integer(Expiration, 16),
                         base64:decode(HashedSecret));
        _ ->
            {error, invalid_token_version}
    end.

%% @doc Check that an API <code>Token</code> has not expired and was encoded
%%      properly with the given <code>Secret</code>, <code>Salt</code> and
%%      <code>Timestamp</code>.
-spec verify(Secret :: binary(), Salt :: binary(), Expiration :: non_neg_integer(),
             HashedSecret :: binary()) -> ok | {error, Reason :: term()}.
verify(Secret, Salt, Expiration, HashedSecret) ->
    Now = os:system_time(seconds),
    if
        %% Make sure that the authentication token is not expired yet.
        Now =< Expiration ->
            case hash_secret(Secret, Salt, Expiration) of
                HashedSecret -> ok;
                _            -> {error, {invalid_token}}
            end;
        true ->
            {error, expired_token}
    end.


-spec rand_bytes(Length :: non_neg_integer()) -> binary().
rand_bytes(Length) when is_integer(Length), Length > 0 ->
    try crypto:strong_rand_bytes(Length) of
        Bytes ->
            Bytes
    catch
        _:low_entropy ->
            crypto:rand_bytes(Length)
    end.
