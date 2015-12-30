%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2015 Cielo24, Inc.
%%% @doc Implementation of password-based key derivation function 2 that
%%%      is compatible with the Python pbkdf2 library. For more information
%%%      see https://github.com/dlitz/python-pbkdf2.
%%% @end
%%%-------------------------------------------------------------------
-module(cet_pbkdf2).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([encode/3, encode/5]).
-export([decode/1]).
-export([verify/2, verify/3]).
-export([hash/3, hash/5]).
-export([secure_compare/2]).
-export([make_salt/1]).

-export_type([hash_option/0]).

-define(LENGTH, 20).
-define(MAX_LENGTH, 16#ffffffff).
-define(ROUNDS, 400).
-define(PREFIX, "p5k2").

-type hash_option() :: {hmac, crypto:hash_algorithm()}
                     | {rounds, non_neg_integer()}
                     | {length, non_neg_integer()}.


-spec encode(Secret :: binary(), Salt :: binary(), [hash_option()]) -> Token :: binary().
encode(Secret, Salt, Options) ->
    MacType = proplists:get_value(hmac, Options, sha),
    Rounds = proplists:get_value(rounds, Options, ?ROUNDS),
    Length = proplists:get_value(length, Options, ?LENGTH),
    encode(MacType, Secret, Salt, Rounds, Length).

-spec encode(crypto:hash_algorithm(), Secret :: binary(), Salt :: binary(),
             Rounds :: non_neg_integer(), Length :: non_neg_integer()) -> Token :: binary().
encode(MacType, Secret, Salt, Rounds, Length) ->
    Hash = base64:encode(hash(MacType, Secret, Salt, Rounds, Length)),
    iolist_to_binary([<<"$" ?PREFIX "$">>, integer_to_binary(Rounds, 16), $$, Salt, $$, Hash]).


-spec decode(Token :: binary()) -> {ok, {Hash :: binary(), Salt :: binary(), Rounds :: non_neg_integer()}} |
                                   {error, Reason :: term()}.
decode(Token) ->
    case binary:split(Token, <<"$">>, [global]) of
        [<<>>, <<?PREFIX>>, HexRounds, Salt, Base64Hash] ->
            Rounds = binary_to_integer(HexRounds, 16),
            Hash = base64:decode(Base64Hash),
            {ok, {Hash, Salt, Rounds}};
        _ ->
            {error, invalid_pbkdf2_token}
    end.


%% @doc Verify that a <code>Token</code> was created with the given <code>Secret</code>.
-spec verify(Token :: binary(), Secret :: binary()) ->
                    ok | {error, Reason :: term()}.
verify(Token, Secret) ->
    verify(Token, sha, Secret).

-spec verify(Token :: binary(), crypto:hash_algorithm(), Secret :: binary()) ->
                    ok | {error, Reason :: term()}.
verify(Token, MacType, Secret) ->
    case decode(Token) of
        {ok, {Hash1, Salt, Rounds}} ->
            Hash2 = hash(MacType, Secret, Salt, Rounds, byte_size(Hash1)),
            case secure_compare(Hash1, Hash2) of
                true  -> ok;
                false -> {error, pbkdf2_key_mismatch}
            end;
        Error ->
            Error
    end.


%% @doc Generate a key derived from an existing secret. The PBKDF2 key
%% derivation function has five input parameters:
%% <code>
%% DK = PBKDF2(PRF, Password, Salt, c, dkLen)
%% </code>
%% where:
%%
%%   - PRF is a pseudorandom function of two parameters with output length hLen
%%     (e.g. a keyed HMAC).
%%   - Password is the master password from which a derived key is generated.
%%   - Salt is a sequence of bits, known as a cryptographic salt.
%%   - c is the number of iterations desired.
%%   - dkLen is the desired length of the derived key.
%%   - DK is the generated derived key.
%%
%% Each hLen-bit block Ti of derived key DK, is computed as follows:
%% <code>
%% DK = T1 || T2 || ... || Tdklen/hlen
%% Ti = F(Password, Salt, c, i)
%% </code>
%% The function F is the xor (^) of c iterations of chained PRFs.
%% The first iteration of PRF uses Password as the PRF key and Salt concatenated
%% with i encoded as a big-endian 32-bit integer. (Note that i is a 1-based index.)
%% Subsequent iterations of PRF use Password as the PRF key and the output of the
%% previous PRF computation as the salt:
%% <code>
%% F(Password, Salt, c, i) = U1 ^ U2 ^ ... ^ Uc
%% </code>
%% where:
%% <code>
%% U1 = PRF(Password, Salt || INT_32_BE(i))
%% U2 = PRF(Password, U1)
%% ...
%% Uc = PRF(Password, Uc-1)
%% </code>
-spec hash(Secret :: binary(), Salt :: binary(), [hash_option()]) -> Hash :: binary().
hash(Secret, Salt, Options) ->
    MacType = proplists:get_value(hmac, Options, sha),
    Rounds = proplists:get_value(rounds, Options, ?ROUNDS),
    Length = proplists:get_value(length, Options, ?LENGTH),
    hash(MacType, Secret, Salt, Rounds, Length).

hash(MacType, Secret, Salt, Rounds, MaxLength)
  when is_atom(MacType), is_binary(Secret), is_binary(Salt),
       is_integer(Rounds), MaxLength < ?MAX_LENGTH ->
    hash(MacType, Secret, Salt, Rounds, MaxLength, 1, []).

hash(MacType, Secret, Salt, Rounds, MaxLength, BlockIndex, Acc) ->
    case iolist_size(Acc) < MaxLength of
        true ->
            Block = hash(MacType, Secret, Salt, Rounds, BlockIndex, 1, <<>>, <<>>),
            hash(MacType, Secret, Salt, Rounds, MaxLength, BlockIndex + 1, [Block | Acc]);
        false ->
            <<Key:MaxLength/binary, _Rest/binary>> = iolist_to_binary(lists:reverse(Acc)),
            Key
    end.

hash(MacType, Secret, Salt, Rounds, BlockIndex, Round, _Prev, _Acc0) when Round =:= 1 ->
    Initial = crypto:hmac(MacType, Secret, <<Salt/binary, BlockIndex:32/unsigned-integer>>),
    hash(MacType, Secret, Salt, Rounds, BlockIndex, Round + 1, Initial, Initial);
hash(MacType, Secret, Salt, Rounds, BlockIndex, Round, Prev, Acc0) when Round =< Rounds ->
    Next = crypto:hmac(MacType, Secret, Prev),
    Acc = crypto:exor(Next, Acc0),
    hash(MacType, Secret, Salt, Rounds, BlockIndex, Round + 1, Next, Acc);
hash(_MacType, _Secret, _Salt, _Rounds, _BlockIndex, _Round, _Prev, Acc) ->
    Acc.


%% @doc Compare two binaries without breaking at the first different character
%% to avoid timing attacks.
-spec secure_compare(binary(), binary()) -> boolean().
secure_compare(Bin1, Bin2) ->
    secure_compare(Bin1, Bin2, 0).

secure_compare(<<Char1, Rest1/binary>>, <<Char2, Rest2/binary>>, Acc) ->
    secure_compare(Rest1, Rest2, Acc bor (Char1 bxor Char2));
secure_compare(_Bin1, _Bin2, Acc) ->
    Acc =:= 0.


-spec make_salt(Length :: non_neg_integer()) -> Salt :: binary().
make_salt(Length) ->
    base64:encode(rand_bytes(Length)).


-spec rand_bytes(Length :: non_neg_integer()) -> binary().
rand_bytes(Length) when is_integer(Length), Length > 0 ->
    try crypto:strong_rand_bytes(Length) of
        Bytes ->
            Bytes
    catch
        _:low_entropy ->
            crypto:rand_bytes(Length)
    end.



-ifdef(TEST).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").


-define(RFC6070_TEST_VECTORS,
        [
         {[sha, <<"password">>, <<"salt">>, 1, 20],
          <<"0c60c80f961f0e71f3a9b524af6012062fe037a6">>},
         {[sha, <<"password">>, <<"salt">>, 2, 20],
          <<"ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957">>},
         {[sha, <<"password">>, <<"salt">>, 4096, 20],
          <<"4b007901b765489abead49d926f721d065a429c1">>},
         {[sha, <<"passwordPASSWORDpassword">>, <<"saltSALTsaltSALTsaltSALTsaltSALTsalt">>, 4096, 25],
          <<"3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038">>},
         {[sha, <<"pass\0word">>, <<"sa\0lt">>, 4096, 16],
          <<"56fa6aa75548099dcc37d7f03425e0c3">>},
         {[sha, <<"password">>, <<"salt">>, 16777216, 20],
          <<"eefe3d61cd4da4e4e9945b3d6ba2158c2634e984">>}
        ]).


pbkdf2_hex(Args) ->
	io:format("Running test with Args = ~p~n", [Args]),
	Key = apply(mdps_pbkdf2, hash, Args),
	bstr:hexencode(Key).


rfc6070_correctness_test_() ->
    [
     {timeout, 60, ?_assertEqual(Expected, pbkdf2_hex(Args))}	|| {Args, Expected} <- ?RFC6070_TEST_VECTORS
		].


-define(RFC3962_TEST_VECTORS,
        [
         {[sha, <<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1, 16],
          <<"cdedb5281bb2f801565a1122b2563515">>},
         {[sha, <<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1, 32],
          <<"cdedb5281bb2f801565a1122b2563515"
            "0ad1f7a04bb9f3a333ecc0e2e1f70837">>},
         {[sha, <<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 2, 16],
          <<"01dbee7f4a9e243e988b62c73cda935d">>},
         {[sha, <<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 2, 32],
          <<"01dbee7f4a9e243e988b62c73cda935d"
            "a05378b93244ec8f48a99e61ad799d86">>},
         {[sha, <<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1200, 16],
          <<"5c08eb61fdf71e4e4ec3cf6ba1f5512b">>},
         {[sha, <<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1200, 32],
          <<"5c08eb61fdf71e4e4ec3cf6ba1f5512b"
            "a7e52ddbc5e5142f708a31e2e62b1e13">>},
         {[sha, <<"password">>, binary:encode_unsigned(16#1234567878563412), 5, 16],
          <<"d1daa78615f287e6a1c8b120d7062a49">>},
         {[sha, <<"password">>, binary:encode_unsigned(16#1234567878563412), 5, 32],
          <<"d1daa78615f287e6a1c8b120d7062a49"
            "3f98d203e6be49a6adf4fa574b6e64ee">>},
         {[sha, <<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
           <<"pass phrase equals block size">>, 1200, 16],
          <<"139c30c0966bc32ba55fdbf212530ac9">>},
         {[sha, <<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
           <<"pass phrase equals block size">>, 1200, 32],
          <<"139c30c0966bc32ba55fdbf212530ac9"
            "c5ec59f1a452f5cc9ad940fea0598ed1">>},
         {[sha, <<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
           <<"pass phrase exceeds block size">>, 1200, 16],
          <<"9ccad6d468770cd51b10e6a68721be61">>},
         {[sha, <<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
           <<"pass phrase exceeds block size">>, 1200, 32],
          <<"9ccad6d468770cd51b10e6a68721be61"
            "1a8b4d282601db3b36be9246915ec82a">>}
        ]).

rfc3962_correctness_test_() ->
	[
   {timeout, 60, ?_assertEqual(Expected, pbkdf2_hex(Args))} || {Args, Expected} <- ?RFC3962_TEST_VECTORS
  ].


-endif.
