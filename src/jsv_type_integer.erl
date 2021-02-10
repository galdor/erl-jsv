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

-module(jsv_type_integer).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/2, validate_constraint/4,
         generate/2]).

-export_type([constraint/0]).

-type constraint() :: {min, number()}
                    | {max, number()}.

verify_constraint({min, Min}, _) when is_integer(Min) ->
  ok;
verify_constraint({min, _}, _) ->
  invalid;
verify_constraint({max, Max}, _) when is_integer(Max) ->
  ok;
verify_constraint({max, _}, _) ->
  invalid;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min, Min}, _) ->
  {"value must be greater or equal to ~0tp", [Min]};

format_constraint_violation({max, Max}, _) ->
  {"value must be lower or equal to ~0tp", [Max]}.

validate_type(Value, _) when is_integer(Value) ->
  ok;
validate_type(_, _) ->
  error.

validate_constraint(Value, {min, Min}, _, _) ->
  Value >= Min;

validate_constraint(Value, {max, Max}, _, _) ->
  Value =< Max.

generate(Term, _) when is_integer(Term) ->
  {ok, Term};
generate(_, _) ->
  error.
