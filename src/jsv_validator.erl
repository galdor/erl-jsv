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

-export([init/3, validate/1, validate_child/4]).

-export_type([state/0]).

-opaque state() :: #{options := jsv:options(),
                     value := json:value(),
                     pointer := json_pointer:pointer(),
                     definition := jsv:definition(),
                     type_map := jsv:type_map(),
                     catalog => jsv:catalog_name()}.

-spec init(json:value(), jsv:definition(), jsv:options()) -> state().
init(Value, Definition, Options) ->
  TypeMap = maps:get(type_map, Options, jsv:default_type_map()),
  #{options => Options,
    value => Value,
    pointer => [],
    definition => Definition,
    type_map => TypeMap}.

-spec validate(state()) ->
        ok | {error, [jsv:value_error()]}.
validate(State = #{definition := TypeName}) when is_atom(TypeName) ->
  validate(State#{definition => {TypeName, #{}}});
validate(State = #{options := Options,
                   definition := {ref, Catalog, DefinitionName}}) ->
  {ok, Definition} = jsv:find_catalog_definition(Options,
                                                 Catalog, DefinitionName),
  validate(State#{definition => Definition,
                  catalog => Catalog});
validate(State = #{definition := {ref, DefinitionName}}) ->
  case maps:find(catalog, State) of
    {ok, Catalog} ->
      validate(State#{definition => {ref, Catalog, DefinitionName}});
    error ->
      {error, [no_current_catalog]}
  end;
validate(State = #{value := Value,
                   definition := {Type, _},
                   type_map := TypeMap}) ->
  Module = maps:get(Type, TypeMap),
  case Module:validate_type(Value) of
    ok ->
      validate_constraints(Value, Module, State);
    {ok, InterpretedValue} ->
      validate_constraints(InterpretedValue, Module, State);
    error ->
      {error, [value_error(State, {invalid_type, Type})]}
  end.

-spec validate_constraints(json:value(), module(), state()) ->
        ok | {error, [jsv:value_error()]}.
validate_constraints(Value, Module,
                     State = #{definition := {Type, Constraints}}) ->
  F = fun (ConstraintName, ConstraintValue, Errors) ->
          Constraint = {ConstraintName, ConstraintValue},
          case Module:validate_constraint(Value, Constraint, State) of
            Result when Result =:= ok; Result =:= true ->
              Errors;
            Result when Result =:= invalid; Result =:= false ->
              Error = constraint_violation(State, Type, Constraint),
              [Error | Errors];
            {invalid, Details} ->
              Error = constraint_violation(State, Type, Constraint,
                                           Details),
              [Error | Errors];
            Errors2 when is_list(Errors2) ->
              Errors2 ++ Errors
          end
      end,
  case maps:fold(F, [], Constraints) of
    [] ->
      ok;
    Errors ->
      {error, Errors}
  end.

-spec validate_child(json:value(), jsv:definition(),
                     json_pointer:reference_token() | undefined,
                     jsv_validator:state()) ->
        ok | {error, [jsv:value_error()]}.
validate_child(Value, Definition, ChildPath,
               State = #{pointer := Pointer}) ->
  Pointer2 = case ChildPath of
                 undefined -> ChildPath;
                 RefToken -> json_pointer:child(Pointer, RefToken)
               end,
  State2 = State#{value => Value,
                  pointer => Pointer2,
                  definition => Definition},
  validate(State2).

-spec constraint_violation(state(), jsv:type(), jsv:constraint()) ->
        jsv:value_error().
constraint_violation(State, Type, Constraint) ->
  value_error(State, {constraint_violation, Type, Constraint}).

-spec constraint_violation(state(), jsv:type(), jsv:constraint(),
                           Details :: term()) ->
        jsv:value_error().
constraint_violation(State, Type, Constraint, Details) ->
  value_error(State, {constraint_violation, Type, Constraint, Details}).

-spec value_error(state(), jsv:value_error_reason()) -> jsv:value_error().
value_error(#{value := Value, pointer := Pointer}, Reason) ->
  #{reason => Reason, value => Value, pointer => Pointer}.
