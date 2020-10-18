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

-module(jsv_type_boolean).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/1,
         validate_type/1, validate_constraint/3]).

-export_type([constraint/0]).

-type constraint() :: {value, boolean()}.

verify_constraint({value, Value}, _) when is_boolean(Value) ->
  ok;
verify_constraint({value, _}, _) ->
  invalid;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({value, ExpectedValue}) ->
  {"value must be ~0tp", [ExpectedValue]}.

validate_type(Value) when is_boolean(Value) ->
  ok;
validate_type(_) ->
  error.

validate_constraint(Value, Constraint = {value, ExpectedValue}, State) ->
  case Value =:= ExpectedValue of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Constraint, boolean, State)
  end.
