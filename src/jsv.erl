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
         verify_definition/2,
         format_value_error/2, format_value_errors/2]).

-export_type([definition/0,
              type/0, type_map/0,
              constraints/0, constraint/0,
              constraint_name/0, constraint_value/0,
              options/0,
              definition_error/0,
              value_error/0, value_error_reason/0,
              constraint_violation_details/0]).

-type definition() :: type() | {type(), constraints()}.

-type type() :: atom().
-type type_map() :: #{type() := module()}.

-type constraints() :: #{constraint_name() := constraint_value()}.
-type constraint() :: {constraint_name(), constraint_value()}.
-type constraint_name() :: atom().
-type constraint_value() :: term().

-type options() :: #{type_map => type_map(),
                     format_value_errors => boolean(),
                     disable_verification => boolean()}.

-type definition_error() :: {invalid_format, term()}
                          | {unknown_type, type()}
                          | {unknown_constraint, jsv:type(), jsv:constraint()}
                          | {invalid_constraint, jsv:type(),
                             jsv:constraint(), term()}.

-type value_error() :: #{reason := value_error_reason(),
                         reason_string => binary(),
                         value := json:value(),
                         pointer := json_pointer:pointer(),
                         pointer_string => binary()}.
-type value_error_reason() :: {invalid_type, ExpectedType :: jsv:type()}
                            | {constraint_violation, jsv:type(), constraint()}
                            | {constraint_violation, jsv:type(), constraint(),
                               constraint_violation_details()}.

-type constraint_violation_details() :: undefined | term().

-spec validate(json:value(), definition()) -> ok | {error, [value_error()]}.
validate(Value, Definition) ->
  validate(Value, Definition, #{}).

-spec validate(json:value(), definition(), options()) ->
        ok | {error, [value_error()]}.
validate(Value, Definition, Options) ->
  TypeMap = maps:get(type_map, Options, default_type_map()),
  case maps:get(disable_verification, Options, false) of
    true ->
      ok;
    false ->
      case verify_definition(Definition, TypeMap) of
        ok ->
          ok;
        {error, DefinitionErrors} ->
          error({invalid_definition, DefinitionErrors})
      end
  end,
  State = jsv_validator:init(Value, Definition, Options),
  case jsv_validator:validate(State) of
    ok ->
      ok;
    {error, Errors} ->
      Errors2 = lists:reverse(Errors),
      Errors3 = case maps:get(format_value_errors, Options, false) of
                  true ->
                    format_value_errors(Errors2, TypeMap);
                  false ->
                    Errors2
                end,
      {error, Errors3}
  end.

-spec verify_definition(definition(), type_map()) ->
        ok | {error, [definition_error()]}.
verify_definition(Definition, TypeMap) when is_atom(Definition) ->
  verify_definition({Definition, #{}}, TypeMap);
verify_definition({TypeName, Constraints}, TypeMap) when
    is_atom(TypeName), is_map(Constraints) ->
  case maps:find(TypeName, TypeMap) of
    {ok, Module} ->
      Exported = lists:member({verify_constraint, 2},
                              Module:module_info(exports)),
      VerifyConstraint = case Exported of
                           true ->
                             fun (C, TM) -> Module:verify_constraint(C, TM) end;
                           false ->
                             fun (_, _) -> unknown end
                         end,
      F = fun (ConstraintName, ConstraintValue, Errors) ->
              Constraint = {ConstraintName, ConstraintValue},
              case VerifyConstraint(Constraint, TypeMap) of
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

-spec format_value_error(value_error(), type_map()) -> value_error().
format_value_error(Error = #{reason := Reason, pointer := Pointer},
                   TypeMap) ->
  Msg = case Reason of
          {invalid_type, ExpectedType} ->
            io_lib:format(<<"value is not of type ~0tp">>, [ExpectedType]);
          {constraint_violation, Type, Constraint} ->
            Module = maps:get(Type, TypeMap),
            case Module:format_constraint_violation(Constraint, undefined) of
              {Format, Args} ->
                iolist_to_binary(io_lib:format(Format, Args));
              Data ->
                unicode:characters_to_binary(Data)
            end;
          {constraint_violation, Type, Constraint, Details} ->
            Module = maps:get(Type, TypeMap),
            case Module:format_constraint_violation(Constraint, Details) of
              {Format, Args} ->
                iolist_to_binary(io_lib:format(Format, Args));
              Data ->
                unicode:characters_to_binary(Data)
            end;
          _ ->
            io_lib:format(<<"invalid value: ~0tp">>, [Reason])
        end,
  Error#{reason_string => iolist_to_binary(Msg),
         pointer_string => json_pointer:serialize(Pointer)}.

-spec format_value_errors([value_error()], type_map()) ->
        [value_error()].
format_value_errors(Errors, TypeMap) ->
  lists:map(fun (Error) -> format_value_error(Error, TypeMap) end, Errors).

-spec default_type_map() -> type_map().
default_type_map() ->
  #{any => jsv_type_any,
    null => jsv_type_null,
    boolean => jsv_type_boolean,
    integer => jsv_type_integer,
    number => jsv_type_number,
    string => jsv_type_string,
    array => jsv_type_array,
    object => jsv_type_object,
    uuid => jsv_type_uuid,
    uri => jsv_type_uri,
    time => jsv_type_time}.
