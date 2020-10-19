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

-module(jsv_type_object).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/1,
         validate_type/1, validate_constraint/3]).

-export_type([constraint/0]).

-type constraint() :: {value_type, jsv:definition()}
                    | {min_size, non_neg_integer()}
                    | {max_size, non_neg_integer()}.

verify_constraint({value_type, Definition}, TypeMap) ->
  jsv:verify_definition(Definition, TypeMap);
verify_constraint({min_size, Min}, _) when is_integer(Min), Min >= 0 ->
  ok;
verify_constraint({min_size, _}, _) ->
  invalid;
verify_constraint({max_size, Max}, _) when is_integer(Max), Max >= 0 ->
  ok;
verify_constraint({max_size, _}, _) ->
  invalid;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min_size, Min}) ->
  {"value must contain at least ~0tp members", [Min]};
format_constraint_violation({max_size, Max}) ->
  {"value must contain at most ~0tp members", [Max]}.

validate_type(Value) when is_map(Value) ->
  ok;
validate_type(_) ->
  error.

validate_constraint(Value, {value_type, ValueType}, State) ->
  F = fun (MemberName, MemberValue, State2) ->
          jsv_validator:validate_child(MemberValue, ValueType,
                                       MemberName, State2)
      end,
  maps:fold(F, State, Value);
validate_constraint(Value, Constraint = {min_size, Min}, State) when
    is_number(Min) ->
  case map_size(Value) >= Min of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Constraint, object, State)
  end;
validate_constraint(Value, Constraint = {max_size, Max}, State) when
    is_number(Max) ->
  case map_size(Value) =< Max of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Constraint, object, State)
  end.
