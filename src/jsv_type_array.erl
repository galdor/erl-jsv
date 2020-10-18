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

-module(jsv_type_array).

-behaviour(jsv_type).

-export([name/0, validate_type/1, validate_constraint/3]).

-type constraint() :: {element_type, jsv:type()}
                    | {min_length, number()}
                    | {max_length, number()}.

-spec name() -> jsv:type().
name() ->
  array.

-spec validate_type(list()) -> ok | error.
validate_type(Value) when is_list(Value) ->
  ok;
validate_type(_) ->
  error.

-spec validate_constraint(list(), constraint(), jsv_validator:state()) ->
        jsv_validator:state().

validate_constraint(Value, {element_type, ElementType}, State) ->
  F = fun (I, Element, State2) ->
          jsv_validator:validate_child(Element, ElementType,
                                       integer_to_binary(I), State2)
      end,
  jsv_utils:fold_list_with_index(F, State, Value);

validate_constraint(Value, Constraint = {min_length, Min}, State) when
    is_number(Min) ->
  case length(Value) >= Min of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Value, Constraint, State)
  end;
validate_constraint(_, Constraint = {min_length, _}, State) ->
  jsv_validator:add_error({invalid_constraint, Constraint}, State);

validate_constraint(Value, Constraint = {max_length, Max}, State) when
    is_number(Max) ->
  case length(Value) =< Max of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Value, Constraint, State)
  end;
validate_constraint(_, Constraint = {max_length, _}, State) ->
  jsv_validator:add_error({invalid_constraint, Constraint}, State);

validate_constraint(_, Constraint, State) ->
  jsv_validator:add_error({invalid_constraint, Constraint}, State).
