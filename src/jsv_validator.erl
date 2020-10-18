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

-module(jsv_validator).

-export([init/3, errors/1, validate/1, validate_child/4,
         add_error/2, add_constraint_violation/3]).

-export_type([state/0]).

-opaque state() :: #{value := json:value(),
                     value_path := json_pointer:pointer(),
                     definition := jsv:definition(),
                     type_map := jsv:type_map(),
                     errors := [jsv:error()]}.

-spec init(json:value(), jsv:definition(), jsv:options()) -> state().
init(Value, Definition, Options) ->
  TypeMap = maps:get(type_map, Options, jsv:default_type_map()),
  #{value => Value,
    value_path => [],
    definition => Definition,
    type_map => TypeMap,
    errors => []}.

-spec errors(state()) -> [jsv:error()].
errors(#{errors := Errors}) ->
  Errors.

-spec validate(state()) -> state().
validate(State = #{definition := Definition}) when is_atom(Definition) ->
  validate(State#{definition => {Definition, #{}}});
validate(State = #{value := Value,
                   definition := {Type, Constraints},
                   type_map := TypeMap}) ->
  case maps:find(Type, TypeMap) of
    {ok, Module} ->
      case Module:validate_type(Value) of
        ok ->
          maps:fold(fun (ConstraintName, ConstraintValue, State2) ->
                        Constraint = {ConstraintName, ConstraintValue},
                        Module:validate_constraint(Value, Constraint, State2)
                    end, State, Constraints);
        error ->
          add_error({invalid_value_type, Value, Type}, State)
      end;
    error ->
      add_error({unknown_type, Type}, State)
  end.

-spec validate_child(json:value(), jsv:definition(),
                     json_pointer:reference_token() | undefined,
                     jsv_validator:state()) -> state().
validate_child(Value, Definition, ChildPath,
               State = #{value_path := ValuePath}) ->
  ValuePath2 = case ChildPath of
                 undefined -> ChildPath;
                 RefToken -> json_pointer:child(ValuePath, RefToken)
               end,
  State2 = State#{value => Value,
                  value_path => ValuePath2,
                  definition => Definition},
  State3 = validate(State2),
  State3#{value_path => ValuePath}.

-spec add_error(jsv:error_reason(), state()) -> state().
add_error(Reason, State = #{errors := Errors}) ->
  Error = validation_error(State, Reason),
  State#{errors => [Error | Errors]}.

-spec add_constraint_violation(json:value(), jsv:constraint(), state()) ->
        state().
add_constraint_violation(Value, Constraint, State = #{errors := Errors}) ->
  Error = validation_error(State, {constraint_violation, Value, Constraint}),
  State#{errors => [Error | Errors]}.

-spec validation_error(state(), jsv:error_reason()) -> jsv:error().
validation_error(#{value_path := ValuePath},
                 Reason) ->
  #{reason => Reason,
    value_path => ValuePath}.
