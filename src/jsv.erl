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

-module(jsv).

-export([default_type_map/0,
         validate/2, validate/3,
         verify_definition/2]).

-export_type([definition/0,
              type/0, type_map/0,
              constraints/0, constraint/0,
              constraint_name/0, constraint_value/0,
              options/0,
              definition_error/0,
              value_error/0, value_error_reason/0]).

-type definition() :: type() | {type(), constraints()}.

-type type() :: atom().
-type type_map() :: #{type() := module()}.

-type constraints() :: #{constraint_name() := constraint_value()}.
-type constraint() :: {constraint_name(), constraint_value()}.
-type constraint_name() :: atom().
-type constraint_value() :: term().

-type options() :: #{type_map => type_map()}.

-type definition_error() :: {invalid_format, term()}
                          | {unknown_type, type()}
                          | {unknown_constraint, jsv:type(), jsv:constraint()}
                          | {invalid_constraint, jsv:type(),
                             jsv:constraint(), term()}.

-type value_error() :: #{reason := value_error_reason(),
                         value := json:value(),
                         value_path := json_pointer:pointer()}.
-type value_error_reason() :: {invalid_type, ExpectedType :: jsv:type()}
                            | {constraint_violation, constraint()}.

-spec validate(json:value(), definition()) -> ok | {error, [value_error()]}.
validate(Value, Definition) ->
  validate(Value, Definition, #{}).

-spec validate(json:value(), definition(), options()) ->
        ok | {error, [value_error()]}.
validate(Value, Definition, Options) ->
  State = jsv_validator:init(Value, Definition, Options),
  State2 = jsv_validator:validate(State),
  case jsv_validator:errors(State2) of
    [] ->
      ok;
    Errors ->
      {error, lists:reverse(Errors)}
  end.

-spec verify_definition(definition(), type_map()) ->
        ok | {error, [definition_error()]}.
verify_definition(Definition, TypeMap) when is_atom(Definition) ->
  verify_definition({Definition, #{}}, TypeMap);
verify_definition({TypeName, Constraints}, TypeMap) ->
  case maps:find(TypeName, TypeMap) of
    {ok, Module} ->
      F = fun (ConstraintName, ConstraintValue, Errors) ->
              Constraint = {ConstraintName, ConstraintValue},
              case Module:verify_constraint(Constraint, TypeMap) of
                ok ->
                  Errors;
                unknown ->
                  [{unknown_constraint, TypeName, Constraint} | Errors];
                invalid ->
                  Error = {invalid_constraint, TypeName, Constraint,
                           invalid_value},
                  [Error | Errors];
                {invalid, Reason} ->
                  Error = {invalid_constraint, TypeName, Constraint, Reason},
                  [Error | Errors];
                {error, ValidationErrors} ->
                  ValidationErrors ++ Errors
              end
          end,
      case maps:fold(F, [], Constraints) of
        [] ->
          ok;
        Errors ->
          {error, Errors}
      end;
    error ->
      {error, [{unknown_type, TypeName}]}
  end;
verify_definition(Definition, _) ->
  {error, [{invalid_format, Definition}]}.

-spec default_type_map() -> type_map().
default_type_map() ->
  #{number => jsv_type_number,
    array => jsv_type_array}.
