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

-module(jsv_type_string).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/1, validate_constraint/4]).

-export_type([constraint/0]).

-type constraint() :: {min_length, non_neg_integer()}
                    | {max_length, non_neg_integer()}
                    | {prefix, binary()}
                    | {suffix, binary()}
                    | {values, [jsv:keyword()]}.

verify_constraint({min_length, Min}, _) when is_integer(Min), Min >= 0 ->
  ok;
verify_constraint({min_length, _}, _) ->
  invalid;

verify_constraint({max_length, Max}, _) when is_integer(Max), Max >= 0 ->
  ok;
verify_constraint({max_length, _}, _) ->
  invalid;

verify_constraint({prefix, Prefix}, _) when is_binary(Prefix) ->
  ok;
verify_constraint({prefix, _}, _) ->
  invalid;

verify_constraint({suffix, Suffix}, _) when is_binary(Suffix) ->
  ok;
verify_constraint({suffix, _}, _) ->
  invalid;

verify_constraint({values, Values}, _) when
    is_list(Values), length(Values) > 0 ->
  case lists:all(fun jsv:is_keyword/1, Values) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({values, _}, _) ->
  invalid;

verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min_length, Min}, _) ->
  {"value must contain at least ~0tp characters", [Min]};

format_constraint_violation({max_length, Max}, _) ->
  {"value must contain at most ~0tp characters", [Max]};

format_constraint_violation({prefix, Prefix}, _) ->
  {"value must start with \"~ts\"", [Prefix]};

format_constraint_violation({suffix, Suffix}, _) ->
  {"value must end with \"~ts\"", [Suffix]};

format_constraint_violation({values, [Keyword]}, _) ->
  String = json:serialize(jsv:keyword_value(Keyword)),
  {"value must be the string \"~ts\"", [String]};
format_constraint_violation({values, Keywords}, _) ->
  Strings = lists:map(fun (K) ->
                          json:serialize(jsv:keyword_value(K))
                      end, Keywords),
  Data = lists:join(<<", ">>, Strings),
  {"value must be one of the following strings: \"~ts\"", [Data]}.

validate_type(Value) when is_binary(Value) ->
  ok;
validate_type(_) ->
  error.

validate_constraint(Value, {min_length, Min}, _, _) ->
  string_length(Value) >= Min;

validate_constraint(Value, {max_length, Max}, _, _) ->
  string_length(Value) =< Max;

validate_constraint(Value, {prefix, Prefix}, _, _) when
    byte_size(Value) < byte_size(Prefix) ->
  invalid;
validate_constraint(Value, {prefix, Prefix}, _, _) ->
  case binary_part(Value, {0, byte_size(Prefix)}) of
    Prefix ->
      ok;
    _ ->
      invalid
  end;

validate_constraint(Value, {suffix, Suffix}, _, _) when
    byte_size(Value) < byte_size(Suffix) ->
  invalid;
validate_constraint(Value, {suffix, Suffix}, _, _) ->
  case binary_part(Value, {byte_size(Value), -byte_size(Suffix)}) of
    Suffix ->
      ok;
    _ ->
      invalid
  end;

validate_constraint(Value, {values, Keywords}, _, _) ->
  F = fun (K) -> jsv:keyword_equal(K, Value) end,
  case lists:any(F, Keywords) of
    true ->
      ok;
    false ->
      invalid
  end.

-spec string_length(binary()) -> non_neg_integer().
string_length(Bin) ->
  string_length(Bin, 0).

-spec string_length(binary(), non_neg_integer()) -> non_neg_integer().
string_length(<<>>, N) ->
  N;
string_length(<<_/utf8, Rest/binary>>, N) ->
  string_length(Rest, N+1).
