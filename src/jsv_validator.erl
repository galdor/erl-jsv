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

-module(jsv_validator).

-export([init/3, validate/1, validate_child/4,
         state_options/1]).

-export_type([state/0, canonicalization_data/0]).

-opaque state() :: #{options := jsv:options(),
                     value := json:value(),
                     pointer := json_pointer:pointer(),
                     definition := jsv:definition(),
                     type_map := jsv:type_map(),
                     catalog => jsv:catalog_name()}.

-type canonicalization_data() :: term().

-spec init(json:value(), jsv:definition(), jsv:options()) -> state().
init(Value, Definition, Options) ->
  TypeMap = maps:get(type_map, Options, jsv:default_type_map()),
  #{options => Options,
    value => Value,
    pointer => [],
    definition => Definition,
    type_map => TypeMap}.

-spec validate(state()) ->
        {ok, term()} | {error, [jsv:value_error()]}.
validate(State = #{definition := {one_of, Definitions}}) when
    is_list(Definitions) ->
  Fun = fun
          F ([]) ->
            {error, [value_error(State, invalid_type)]};
          F ([Def | Defs]) ->
            case validate(State#{definition := Def}) of
              {ok, Value} ->
                {ok, Value};
              {error, _} ->
                F(Defs)
            end
        end,
  Fun(Definitions);
validate(State = #{definition := {ref, DefinitionName}}) ->
  case maps:find(catalog, State) of
    {ok, Catalog} ->
      validate(State#{definition => {ref, Catalog, DefinitionName}});
    error ->
      {error, [no_current_catalog]}
  end;
validate(State = #{definition := {ref, Catalog, DefinitionName}}) ->
  {ok, Definition} = jsv:find_catalog_definition(Catalog, DefinitionName),
  validate(State#{definition => Definition,
                  catalog => Catalog});
validate(State = #{definition := TypeName}) when is_atom(TypeName) ->
  validate(State#{definition => {TypeName, #{}, #{}}});
validate(State = #{definition := {TypeName, Constraints}}) ->
  validate(State#{definition => {TypeName, Constraints, #{}}});
validate(State = #{value := Value,
                   definition := (Definition = {TypeName, _, _}),
                   type_map := TypeMap}) ->
  Module = maps:get(TypeName, TypeMap),
  ValidateValue =
    fun (InputValue, CData) ->
        case
          validate_constraints(InputValue, Module, CData, State)
        of
          {ok, CData2} ->
            Term = canonicalize(Module, InputValue, CData2, State),
            maybe_extra_validate(Term, Definition);
          {error, Errors} ->
            {error, Errors}
        end
    end,
  case Module:validate_type(Value, State) of
    ok ->
      ValidateValue(Value, undefined);
    {ok, Value2} ->
      ValidateValue(Value2, undefined);
    {ok, Value2, CData} ->
      ValidateValue(Value2, CData);
    error ->
      {error, [value_error(State, {invalid_type, TypeName})]}
  end.

-spec canonicalize(module(), json:value(),
                   jsv_validator:canonicalization_data(),
                   jsv_validator:state()) -> term().
canonicalize(Module, Value, CData, State) ->
  case
    jsv_utils:call_if_defined(Module, canonicalize, [Value, CData, State])
  of
    {ok, Value2} ->
      Value2;
    undefined ->
      Value
  end.

-spec maybe_extra_validate(term(), jsv:definition()) ->
        {ok, term()} | {error, [jsv:value_error()]}.
maybe_extra_validate(Term, {_, _, #{validate := Validate}}) ->
  case Validate(Term) of
    {ok, Term2} ->
      {ok, Term2};
    {error, Error} ->
      {error, validation_error_to_value_errors(Error)}
  end;
maybe_extra_validate(Term, _) ->
  {ok, Term}.

-spec validation_error_to_value_errors(jsv:validation_error()) ->
        [jsv:value_error()].
validation_error_to_value_errors(Errors) when is_list(Errors) ->
  lists:flatten(lists:map(fun validation_error_to_value_errors/1, Errors));
validation_error_to_value_errors({invalid_value, Value,
                                  Reason, ReasonString}) ->
  validation_error_to_value_errors({invalid_value, Value, [],
                                    Reason, ReasonString});
validation_error_to_value_errors({invalid_value, Value, ChildPointer,
                                  Reason, ReasonString}) ->
  [#{reason => {invalid_value, Reason, ReasonString},
     reason_string => ReasonString,
     value => Value,
     pointer => ChildPointer}];
validation_error_to_value_errors({invalid_child, ChildPointer, Errors}) ->
  lists:map(fun (Error = #{pointer := ErrorPointer}) ->
                Error#{pointer =>
                         json_pointer:child(ChildPointer, ErrorPointer)}
            end, Errors).

-spec validate_constraints(json:value(), module(), canonicalization_data(),
                           state()) ->
        {ok, canonicalization_data()} | {error, [jsv:value_error()]}.
validate_constraints(Value, Module, CData,
                     State = #{definition := {Type, Constraints, _Extra}}) ->
  F = fun (ConstraintName, ConstraintValue, {CData2, Errors}) ->
          Constraint = {ConstraintName, ConstraintValue},
          case Module:validate_constraint(Value, Constraint, CData2, State) of
            {ok, CData3} ->
              {CData3, Errors};
            Result when Result =:= ok; Result =:= true ->
              {CData2, Errors};
            Result when Result =:= invalid; Result =:= false ->
              Error = constraint_violation(State, Type, Constraint),
              {CData2, [Error | Errors]};
            {invalid, Details} ->
              Error = constraint_violation(State, Type, Constraint, Details),
              {CData2, [Error | Errors]};
            Errors2 when is_list(Errors2) ->
              {CData2, Errors2 ++ Errors}
          end
      end,
  case maps:fold(F, {CData, []}, Constraints) of
    {CData4, []} ->
      {ok, CData4};
    {_, Errors} ->
      {error, Errors}
  end.

-spec validate_child(json:value(), jsv:definition(),
                     json_pointer:reference_token() | undefined,
                     state()) ->
        {ok, json:value()} | {error, [jsv:value_error()]}.
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

-spec state_options(state()) -> jsv:options().
state_options(#{options := Options}) ->
  Options.
