%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(jsv_test).

-include_lib("eunit/include/eunit.hrl").

validate_any_test_() ->
  [?_assertEqual(ok, jsv:validate(null, any)),
   ?_assertEqual(ok, jsv:validate(true, any)),
   ?_assertEqual(ok, jsv:validate(42, any)),
   ?_assertEqual(ok, jsv:validate(3.14, any)),
   ?_assertEqual(ok, jsv:validate(<<"foo">>, any)),
   ?_assertEqual(ok, jsv:validate([1, 2, 3], any)),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1}, any)),
   ?_assertEqual(ok, jsv:validate(42, {any, #{value => 42}})),
   ?_assertEqual(ok, jsv:validate([1, 2, 3], {any, #{value => [1, 2, 3]}})),
   ?_assertMatch({error, _}, jsv:validate(true, {any, #{value => 42}})),
   ?_assertMatch({error, _}, jsv:validate([1], {any, #{value => []}}))].

validate_null_test_() ->
  [?_assertEqual(ok, jsv:validate(null, null)),
   ?_assertMatch({error, _}, jsv:validate(42, null)),
   ?_assertMatch({error, _}, jsv:validate([null], null))].

validate_boolean_test_() ->
  [?_assertEqual(ok, jsv:validate(true, boolean)),
   ?_assertEqual(ok, jsv:validate(false, boolean)),
   ?_assertMatch({error, _}, jsv:validate(null, boolean))].

validate_number_test_() ->
  [?_assertEqual(ok, jsv:validate(42, number)),
   ?_assertEqual(ok, jsv:validate(-1, number)),
   ?_assertEqual(ok, jsv:validate(5.0, number)),
   ?_assertEqual(ok, jsv:validate(3.14, number)),
   ?_assertEqual(ok, jsv:validate(10, {number, #{min => 10}})),
   ?_assertEqual(ok, jsv:validate(42, {number, #{min => 10}})),
   ?_assertEqual(ok, jsv:validate(10.0, {number, #{min => 10}})),
   ?_assertEqual(ok, jsv:validate(10, {number, #{max => 10.5}})),
   ?_assertEqual(ok, jsv:validate(10.5, {number, #{max => 10.5}})),
   ?_assertEqual(ok, jsv:validate(-1, {number, #{max => 10.5}})),
   ?_assertEqual(ok, jsv:validate(4, {number, #{min => 3, max => 5}})),
   ?_assertEqual(ok, jsv:validate(3, {number, #{min => 3, max => 5}})),
   ?_assertEqual(ok, jsv:validate(5, {number, #{min => 3, max => 5}})),
   ?_assertMatch({error, _}, jsv:validate(42, {number, #{min => 100}})),
   ?_assertMatch({error, _}, jsv:validate(5, {number, #{max => 0}})),
   ?_assertMatch({error, _}, jsv:validate(2, {number, #{min => 3, max => 5}})),
   ?_assertMatch({error, _}, jsv:validate(6, {number, #{min => 3, max => 5}}))].

validate_integer_test_() ->
  [?_assertEqual(ok, jsv:validate(42, integer)),
   ?_assertEqual(ok, jsv:validate(-1, integer)),
   ?_assertMatch({error, _}, jsv:validate(3.0, integer)),
   ?_assertMatch({error, _}, jsv:validate(3.14, integer)),
   ?_assertEqual(ok, jsv:validate(4, {integer, #{min => 3, max => 5}})),
   ?_assertEqual(ok, jsv:validate(3, {integer, #{min => 3, max => 5}})),
   ?_assertEqual(ok, jsv:validate(5, {integer, #{min => 3, max => 5}})),
   ?_assertMatch({error, _}, jsv:validate(2, {integer, #{min => 3, max => 5}})),
   ?_assertMatch({error, _}, jsv:validate(6, {integer, #{min => 3, max => 5}})),
   ?_assertMatch({error, _}, jsv:validate(42, {integer, #{min => 100}})),
   ?_assertMatch({error, _}, jsv:validate(5, {integer, #{max => 0}}))].

validate_string_test_() ->
  [?_assertEqual(ok, jsv:validate(<<"">>, string)),
   ?_assertEqual(ok, jsv:validate(<<"foo">>, string)),
   ?_assertEqual(ok, jsv:validate(<<"été"/utf8>>, string)),
   ?_assertEqual(ok, jsv:validate(<<"foo">>, {string, #{min_length => 3}})),
   ?_assertEqual(ok, jsv:validate(<<"foobar">>, {string, #{min_length => 3}})),
   ?_assertMatch({error, _}, jsv:validate(<<"fo">>,
                                          {string, #{min_length => 3}})),
   ?_assertEqual(ok, jsv:validate(<<"foo">>, {string, #{max_length => 3}})),
   ?_assertEqual(ok, jsv:validate(<<"f">>, {string, #{max_length => 3}})),
   ?_assertMatch({error, _}, jsv:validate(<<"foobar">>,
                                          {string, #{max_length => 3}})),
   ?_assertEqual(ok, jsv:validate(<<"">>, {string, #{prefix => <<"">>}})),
   ?_assertEqual(ok, jsv:validate(<<"foo">>, {string, #{prefix => <<"">>}})),
   ?_assertEqual(ok, jsv:validate(<<"foo">>, {string, #{prefix => <<"foo">>}})),
   ?_assertEqual(ok, jsv:validate(<<"foobar">>,
                                  {string, #{prefix => <<"foo">>}})),
   ?_assertMatch({error, _}, jsv:validate(<<"fob">>,
                                          {string, #{prefix => <<"foo">>}})),
   ?_assertMatch({error, _}, jsv:validate(<<"bar">>,
                                          {string, #{prefix => <<"foo">>}})),
   ?_assertEqual(ok, jsv:validate(<<"">>, {string, #{suffix => <<"">>}})),
   ?_assertEqual(ok, jsv:validate(<<"bar">>, {string, #{suffix => <<"">>}})),
   ?_assertEqual(ok, jsv:validate(<<"bar">>, {string, #{suffix => <<"bar">>}})),
   ?_assertEqual(ok, jsv:validate(<<"foobar">>,
                                  {string, #{suffix => <<"bar">>}})),
   ?_assertMatch({error, _}, jsv:validate(<<"foo">>,
                                          {string, #{suffix => <<"bar">>}})),
   ?_assertMatch({error, _}, jsv:validate(<<"aar">>,
                                          {string, #{suffix => <<"bar">>}})),
   ?_assertMatch(ok, jsv:validate(<<"foo">>,
                                  {string, #{values => [<<"foo">>]}})),
   ?_assertMatch(ok, jsv:validate(<<"foo">>,
                                  {string, #{values => [foo, "bar"]}})),
   ?_assertMatch({error, _}, jsv:validate(<<"foo">>,
                                          {string, #{values => [a, "b"]}}))].

validate_array_test_() ->
  [?_assertEqual(ok, jsv:validate([], array)),
   ?_assertEqual(ok, jsv:validate([1, 2, 3], array)),
   ?_assertEqual(ok, jsv:validate([[], []], array)),
   ?_assertEqual(ok, jsv:validate([1, 2, 3], {array, #{min_length => 3}})),
   ?_assertEqual(ok, jsv:validate([1, 2, 3, 4], {array, #{min_length => 3}})),
   ?_assertMatch({error, _}, jsv:validate([], {array, #{min_length => 3}})),
   ?_assertMatch({error, _}, jsv:validate([1, 2], {array, #{min_length => 3}})),
   ?_assertEqual(ok, jsv:validate([1, 2, 3], {array, #{max_length => 3}})),
   ?_assertEqual(ok, jsv:validate([], {array, #{max_length => 3}})),
   ?_assertMatch({error, _}, jsv:validate([1, 2, 3, 4],
                                          {array, #{max_length => 3}})),
   ?_assertEqual(ok, jsv:validate([], {array, #{element => integer}})),
   ?_assertEqual(ok, jsv:validate([1, 2, 3],
                                  {array, #{element => integer}})),
   ?_assertMatch({error, _}, jsv:validate([true],
                                          {array, #{element => integer}})),
   ?_assertMatch({error, _}, jsv:validate([1, 2, null],
                                          {array, #{element => integer}})),
   ?_assertEqual(ok, jsv:validate([[], [1, 2.5], [3.0]],
                                  {array, #{element =>
                                              {array, #{element =>
                                                          number}}}})),
   ?_assertMatch({error, _}, jsv:validate([0, [1, 2.5], [3.0]],
                                          {array, #{element =>
                                                      {array, #{element =>
                                                                  number}}}})),
   ?_assertEqual(ok, jsv:validate([], {array, #{element =>
                                                  {array, #{element =>
                                                              number}}}}))].

validate_object_test_() ->
  [?_assertEqual(ok, jsv:validate(#{}, object)),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1}, object)),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1, <<"b">> => 2},
                                  {object, #{min_size => 2}})),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3},
                                  {object, #{min_size => 2}})),
   ?_assertMatch({error, _}, jsv:validate(#{},
                                          {object, #{min_size => 2}})),
   ?_assertMatch({error, _}, jsv:validate(#{<<"a">> => 1},
                                          {object, #{min_size => 2}})),
   ?_assertEqual(ok, jsv:validate(#{}, {object, #{max_size => 2}})),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1, <<"b">> => 2},
                                  {object, #{max_size => 2}})),
   ?_assertMatch({error, _}, jsv:validate(#{<<"a">> => 1, <<"b">> => 2,
                                            <<"c">> => 3},
                                          {object, #{max_size => 2}})),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1},
                                  {object, #{value_type => integer}})),
   ?_assertEqual(ok, jsv:validate(#{},
                                  {object, #{value_type => integer}})),
   ?_assertMatch({error, _}, jsv:validate(#{<<"a">> => true, <<"b">> => 42},
                                          {object, #{value_type => integer}})),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1, <<"b">> => 2},
                                  {object, #{required => [<<"a">>, <<"b">>]}})),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3},
                                  {object, #{required => [<<"a">>, <<"b">>]}})),
   ?_assertMatch({error, _}, jsv:validate(#{<<"a">> => 1, <<"c">> => 3},
                                          {object, #{required =>
                                                       [<<"a">>, <<"b">>]}})),
   ?_assertMatch({error, _}, jsv:validate(#{<<"c">> => 3},
                                          {object, #{required =>
                                                       [<<"a">>, <<"b">>]}})),
   ?_assertEqual(ok, jsv:validate(#{}, {object, #{required => []}})),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1, <<"b">> => true},
                                  {object, #{members =>
                                               #{<<"a">> => integer,
                                                 <<"b">> => boolean}}})),
   ?_assertEqual(ok, jsv:validate(#{<<"a">> => 1},
                                  {object, #{members =>
                                               #{<<"a">> => integer,
                                                 <<"b">> => boolean}}})),
   ?_assertEqual(ok, jsv:validate(#{},
                                  {object, #{members =>
                                               #{<<"a">> => integer,
                                                 <<"b">> => boolean}}})),
   ?_assertMatch({error, _}, jsv:validate(#{<<"a">> => 1, <<"b">> => true},
                                          {object, #{members =>
                                                       #{<<"a">> => integer,
                                                         <<"b">> => null}}})),
   ?_assertMatch(ok, jsv:validate(#{<<"a">> => 1, <<"b">> => true},
                                  {object, #{members =>
                                               #{"a" => integer,
                                                 b => boolean},
                                             required =>
                                               [a, "b"]}}))].

validate_uuid_test_() ->
  [?_assertEqual(ok, jsv:validate(<<"cab4cb80-4c66-4ed4-b248-7b52925130f1">>,
                                  uuid)),
   ?_assertEqual(ok, jsv:validate(<<"CAB4CB80-4C66-4ED4-B248-7B52925130F1">>,
                                  uuid)),
   ?_assertMatch({error, _}, jsv:validate(<<"">>, uuid)),
   ?_assertMatch({error, _}, jsv:validate(<<"foobar">>, uuid)),
   ?_assertMatch({error, _},
                 jsv:validate(<<"xyb4cb80-4c66-4ed4-b248-7b52925130f1">>,
                              uuid)),
   ?_assertMatch({error, _},
                 jsv:validate(<<"cab4cb80-4c66-4ed4-b248-7b52925130f1-12">>,
                              uuid))].

validate_uri_test_() ->
  [?_assertEqual(ok, jsv:validate(<<"http://example.com">>, uri)),
   ?_assertEqual(ok, jsv:validate(<<"/foo/bar">>, uri)),
   ?_assertEqual(ok, jsv:validate(<<"//example.com?a=b#foo">>, uri)),
   ?_assertMatch({error, _}, jsv:validate(<<":http">>, uri)),
   ?_assertMatch({error, _}, jsv:validate(<<"">>, uri))].

validate_time_test_() ->
  [?_assertEqual(ok, jsv:validate(<<"10:20:30">>, time)),
   ?_assertEqual(ok, jsv:validate(<<"00:00:00">>, time)),
   ?_assertEqual(ok, jsv:validate(<<"23:59:59">>, time)),
   ?_assertEqual(ok, jsv:validate(<<"23:59:60">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"10">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"10:20">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"10:20:3">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"1:20:30">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"102030">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"1a:20:30">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"10:-2:30">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"24:00:00">>, time)),
   ?_assertMatch({error, _}, jsv:validate(<<"10:60:00">>, time)),
   ?_assertEqual(ok, jsv:validate(<<"10:20:30">>,
                                  {time, #{min => {10, 0, 0}}})),
   ?_assertEqual(ok, jsv:validate(<<"10:20:30">>,
                                  {time, #{min => {10, 20, 30}}})),
   ?_assertMatch({error, _}, jsv:validate(<<"10:20:30">>,
                                          {time, #{min => {10, 20, 31}}})),
   ?_assertEqual(ok, jsv:validate(<<"10:20:30">>,
                                  {time, #{max => {11, 0, 0}}})),
   ?_assertEqual(ok, jsv:validate(<<"11:00:00">>,
                                  {time, #{max => {11, 0, 0}}})),
   ?_assertMatch({error, _}, jsv:validate(<<"11:00:01">>,
                                          {time, #{max => {11, 0, 0}}}))].

validate_date_test_() ->
  [?_assertEqual(ok, jsv:validate(<<"2010-01-01">>, date)),
   ?_assertEqual(ok, jsv:validate(<<"2010-12-31">>, date)),
   ?_assertEqual(ok, jsv:validate(<<"2012-02-29">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"2013-02-29">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"2012-01-32">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"2012-13-01">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"foo">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"2010">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"2010-01">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"2010-01-">>, date)),
   ?_assertMatch({error, _}, jsv:validate(<<"-01-01">>, date)),
   ?_assertEqual(ok, jsv:validate(<<"2010-01-01">>,
                                  {date, #{min => {2010, 1, 1}}})),
   ?_assertEqual(ok, jsv:validate(<<"2010-12-31">>,
                                  {date, #{min => {2010, 1, 1}}})),
   ?_assertMatch({error, _}, jsv:validate(<<"2009-12-31">>,
                                          {date, #{min => {2010, 1, 1}}})),
   ?_assertEqual(ok, jsv:validate(<<"2010-01-01">>,
                                  {date, #{max => {2010, 1, 1}}})),
   ?_assertEqual(ok, jsv:validate(<<"2009-12-31">>,
                                  {date, #{max => {2010, 1, 1}}})),
   ?_assertMatch({error, _}, jsv:validate(<<"2010-01-02">>,
                                          {date, #{max => {2010, 1, 1}}}))].

validate_datetime_test_() ->
  [?_assertEqual(ok, jsv:validate(<<"2010-01-01T00:00:00Z">>, datetime)),
   ?_assertEqual(ok, jsv:validate(<<"2010-12-31T23:00:00+02:00">>, datetime)),
   ?_assertEqual(ok, jsv:validate(<<"2010-02-28t05:00:60-01:45">>, datetime)),
   ?_assertMatch({error, _},
                 jsv:validate(<<"2010-01-01T00:00:00">>, datetime)),
   ?_assertMatch({error, _},
                 jsv:validate(<<"00:00:00">>, datetime)),
   ?_assertMatch({error, _},
                 jsv:validate(<<"2010-01-01">>, datetime)),
   ?_assertEqual(ok, jsv:validate(<<"2010-01-01T12:00:00Z">>,
                                  {datetime, #{min => {{2010, 1, 1},
                                                       {12, 0, 0}}}})),
   ?_assertMatch({error, _}, jsv:validate(<<"2010-01-01T12:00:00Z">>,
                                          {datetime, #{min => {{2010, 1, 1},
                                                               {12, 0, 1}}}})),
   ?_assertMatch({error, _}, jsv:validate(<<"2010-01-01T12:00:00Z">>,
                                          {datetime, #{min => {{2010, 1, 2},
                                                               {10, 0, 0}}}})),
   ?_assertEqual(ok, jsv:validate(<<"2010-01-01T11:00:00-06:00">>,
                                  {datetime, #{min => {{2010, 1, 1},
                                                       {12, 0, 0}}}})),
   ?_assertMatch({error, _},
                 jsv:validate(<<"2010-01-01T11:59:59Z">>,
                              {datetime, #{min => {{2010, 1, 1},
                                                   {12, 0, 0}}}})),
   ?_assertMatch({error, _},
                 jsv:validate(<<"2010-01-01T13:00:00+04:00">>,
                              {datetime, #{min => {{2010, 1, 1},
                                                   {12, 0, 0}}}})),
   ?_assertEqual(ok, jsv:validate(<<"2010-01-01T12:00:00Z">>,
                                  {datetime, #{max => {{2010, 1, 1},
                                                       {12, 0, 0}}}})),
   ?_assertEqual(ok, jsv:validate(<<"2010-01-01T11:59:59Z">>,
                                  {datetime, #{max => {{2010, 1, 1},
                                                       {12, 0, 0}}}})),
   ?_assertEqual(ok, jsv:validate(<<"2009-12-31T13:00:00Z">>,
                                  {datetime, #{max => {{2010, 1, 1},
                                                       {12, 0, 0}}}})),
   ?_assertMatch({error, _},
                 jsv:validate(<<"2010-01-01T12:00:01Z">>,
                              {datetime, #{max => {{2010, 1, 1},
                                                   {12, 0, 0}}}})),
   ?_assertMatch({error, _},
                 jsv:validate(<<"2010-01-02T12:00:00Z">>,
                              {datetime, #{max => {{2010, 1, 1},
                                                   {12, 0, 0}}}})),
   ?_assertMatch({error, _},
                 jsv:validate(<<"2010-01-01T11:00:00-03:00">>,
                              {datetime, #{max => {{2010, 1, 1},
                                                   {12, 0, 0}}}})),
   ?_assertEqual(ok,
                 jsv:validate(<<"2010-01-01T13:00:00+02:00">>,
                              {datetime, #{max => {{2010, 1, 1},
                                                   {12, 0, 0}}}}))].

validate_catalogs_test_() ->
  [?_assertError({invalid_definition, [{unknown_catalog, a}]},
                 jsv:validate(42, {definition, a, b})),
   ?_assertError({invalid_definition, [{unknown_definition, a, b}]},
                 jsv:validate(42, {definition, a, b},
                              #{catalogs => #{a => #{}}})),
   ?_assertEqual(ok, jsv:validate(42, {definition, a, b},
                                  #{catalogs => #{a => #{b => integer}}})),
   ?_assertMatch({error, _},
                 jsv:validate(42, {definition, a, b},
                              #{catalogs => #{a => #{b => string}}})),
   ?_assertError({invalid_definition, [{unknown_definition, a, c}]},
                 jsv:validate(42, {definition, a, b},
                              #{catalogs => #{a => #{b => {definition, c}}}})),
   ?_assertError({invalid_definition, [{unknown_definition, a, d}]},
                 jsv:validate(42, {definition, a, b},
                              #{catalogs => #{a => #{b => {definition, c},
                                                     c => {definition, d}}}})),
   ?_assertEqual(ok, jsv:validate(42, {definition, a, b},
                                  #{catalogs => #{a => #{b => {definition, c},
                                                         c => {definition, d},
                                                         d => integer}}}))].
