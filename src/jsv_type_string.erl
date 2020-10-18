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

-export([verify_constraint/2, format_constraint_violation/1,
         validate_type/1, validate_constraint/3]).

-export_type([constraint/0]).

-type constraint() :: {min_length, number()}
                    | {max_length, number()}.

verify_constraint({min_length, Min}, _) when is_number(Min) ->
  ok;
verify_constraint({min_length, _}, _) ->
  invalid;
verify_constraint({max_length, Max}, _) when is_number(Max) ->
  ok;
verify_constraint({max_length, _}, _) ->
  invalid;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min_length, Min}) ->
  {"value must contain at least ~0tp characters", [Min]};
format_constraint_violation({max_length, Max}) ->
  {"value must contain at most ~0tp characters", [Max]}.

validate_type(Value) when is_binary(Value) ->
  ok;
validate_type(_) ->
  error.

validate_constraint(Value, Constraint = {min_length, Min}, State) when
    is_number(Min) ->
  case string_length(Value) >= Min of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Constraint, array, State)
  end;
validate_constraint(Value, Constraint = {max_length, Max}, State) when
    is_number(Max) ->
  case string_length(Value) =< Max of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Constraint, array, State)
  end.

-spec string_length(binary()) -> non_neg_integer().
string_length(Bin) ->
  string_length(Bin, 0).

-spec string_length(binary(), non_neg_integer()) -> non_neg_integer().
string_length(<<>>, N) ->
  N;
string_length(<<_/utf8, Rest/binary>>, N) ->
  string_length(Rest, N+1).
