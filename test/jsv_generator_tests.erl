%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(jsv_generator_tests).

-include_lib("eunit/include/eunit.hrl").

generate_any_test_() ->
  [?_assertEqual({ok, null},
                 jsv:generate(null, any)),
   ?_assertEqual({ok, 42},
                 jsv:generate(42, any)),
   ?_assertEqual({ok, <<"foo">>},
                 jsv:generate(<<"foo">>, any)),
   ?_assertEqual({ok, [1, 2, 3]},
                 jsv:generate([1, 2, 3], any)),
   ?_assertEqual({ok, #{a => 1, <<"b">> => true}},
                 jsv:generate(#{a => 1, <<"b">> => true}, any))].

generate_null_test_() ->
  [?_assertEqual({ok, null},
                 jsv:generate(null, null)),
   ?_assertEqual({error, {invalid_value, 42, null}},
                 jsv:generate(42, null))].

generate_boolean_test_() ->
  [?_assertEqual({ok, true},
                 jsv:generate(true, boolean)),
   ?_assertEqual({error, {invalid_value, 42, boolean}},
                 jsv:generate(42, boolean))].

generate_number_test_() ->
  [?_assertEqual({ok, 42},
                 jsv:generate(42, number)),
   ?_assertEqual({ok, 3.14},
                 jsv:generate(3.14, number)),
   ?_assertEqual({error, {invalid_value, <<"foo">>, number}},
                 jsv:generate(<<"foo">>, number))].

generate_integer_test_() ->
  [?_assertEqual({ok, 42},
                 jsv:generate(42, integer)),
   ?_assertEqual({error, {invalid_value, true, integer}},
                 jsv:generate(true, integer))].

generate_string_test_() ->
  [?_assertEqual({ok, <<"foo">>},
                 jsv:generate(<<"foo">>, string)),
   ?_assertEqual({ok, <<"foo">>},
                 jsv:generate("foo", string)),
   ?_assertEqual({ok, <<"été"/utf8>>},
                 jsv:generate("été", string)),
   ?_assertEqual({ok, <<"foo">>},
                 jsv:generate(foo, string)),
   ?_assertEqual({ok, <<"été"/utf8>>},
                 jsv:generate('été', string)),
   ?_assertEqual({ok, <<"foo">>},
                 jsv:generate(foo, {string, #{values => [foo, bar]}}))].

generate_array_test_() ->
  [?_assertEqual({ok, [1, 2, 3]},
                 jsv:generate([1, 2, 3], array)),
   ?_assertEqual({ok, [1, 2, 3]},
                 jsv:generate([1, 2, 3], {array, #{element => integer}})),
   ?_assertEqual({error, {invalid_value, 1, array}},
                 jsv:generate(1, array)),
   ?_assertEqual({error, {invalid_value, true, integer}},
                 jsv:generate([1, true, 3], {array, #{element => integer}}))].

generate_object_test_() ->
  [?_assertEqual({ok, #{<<"a">> => 1, <<"b">> => {data, <<"[]">>}}},
                 jsv:generate(#{a => 1, <<"b">> => {data, <<"[]">>}}, object)),
   ?_assertEqual({ok, #{<<"a">> => <<"foo">>, <<"b">> => <<"bar">>}},
                 jsv:generate(#{a => foo, b => "bar"},
                              {object, #{value => string}})),
   ?_assertEqual({ok, #{<<"a">> => <<"foo">>, <<"b">> => <<"2020-12-04">>}},
                 jsv:generate(#{a => foo, b => {2020, 12, 4}},
                              {object, #{members => #{a => string,
                                                      "b" => date}}})),
   ?_assertEqual({error, {invalid_value, [1, 2, 3], object}},
                 jsv:generate([1, 2, 3], object)),
   ?_assertEqual({error, {invalid_value, true, integer}},
                 jsv:generate(#{a => 1, b => true},
                              {object, #{value => integer}})),
   ?_assertEqual({error, {invalid_value, {1, 2}, date}},
                 jsv:generate(#{a => <<"">>, b => {1, 2}},
                              {object, #{members => #{a => string,
                                                      "b" => date}}}))].

generate_uuid_test_() ->
  [?_assertEqual({ok, <<"da0d1078-d2cb-4001-9009-0bb953f75dcb">>},
                 jsv:generate(<<218,13,16,120,210,203,64,1,
                                144,9,11,185,83,247,93,203>>, uuid)),
   ?_assertEqual({error, {invalid_value, <<"foo">>, uuid}},
                 jsv:generate(<<"foo">>, uuid))].

generate_ksuid_test_() ->
  [?_assertEqual({ok, <<"1lBXqnSQoSzv6kZpygOXoQVIyzi">>},
                 jsv:generate(<<"1lBXqnSQoSzv6kZpygOXoQVIyzi">>, ksuid)),
   ?_assertEqual({error, {invalid_value, <<"foo">>, ksuid}},
                 jsv:generate(<<"foo">>, ksuid))].

generate_uri_test_() ->
  [?_assertEqual({ok, <<"http://example.com">>},
                 jsv:generate(<<"http://example.com">>, uri)),
   ?_assertEqual({error, {invalid_value, 42, uri}},
                 jsv:generate(42, uri))].

generate_time_test_() ->
  [?_assertEqual({ok, <<"10:01:30">>},
                 jsv:generate({10, 1, 30}, time)),
   ?_assertEqual({error, {invalid_value, {10, 1}, time}},
                 jsv:generate({10, 1}, time))].

generate_date_test_() ->
  [?_assertEqual({ok, <<"2020-12-04">>},
                 jsv:generate({2020, 12, 4}, date)),
   ?_assertEqual({error, {invalid_value, {2020, 12}, date}},
                 jsv:generate({2020, 12}, date))].

generate_datetime_test_() ->
  [?_assertEqual({ok, <<"2020-12-04T10:01:30Z">>},
                 jsv:generate({{2020, 12, 4}, {10, 1, 30}}, datetime)),
   ?_assertEqual({error, {invalid_value, {2020, 12, 4}, datetime}},
                 jsv:generate({2020, 12, 4}, datetime)),
   ?_assertEqual({error, {invalid_value, {{2020, 12, 4}, 42}, datetime}},
                 jsv:generate({{2020, 12, 4}, 42}, datetime))].

extra_generate_test_() ->
  GenerateScore = fun
                    ({score, N}) when N >= 0 ->
                      {ok, <<"score:", (integer_to_binary(N))/binary>>};
                    (_) ->
                      {error, invalid_score}
                  end,
  Def = {string, #{}, #{generate => GenerateScore}},
  [?_assertMatch({ok, <<"score:42">>},
                 jsv:generate({score, 42}, Def)),
   ?_assertMatch({error,{invalid_value, invalid_score}},
                 jsv:generate({score, -123}, Def))].
