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

-module(jsv_verifier).

-export([init/2, verify/1]).

-type state() :: #{options := jsv:options(),
                   definition := jsv:definition(),
                   catalog => jsv:catalog_name()}.

-spec init(jsv:definition(), jsv:options()) -> state().
init(Definition, Options) ->
  #{options => Options,
    definition => Definition}.

-spec verify(state()) -> ok | {error, [jsv:definition_error()]}.
verify(State = #{definition := TypeName}) when
    is_atom(TypeName) ->
  verify(State#{definition => {TypeName, #{}}});
verify(State = #{definition := {ref, DefinitionName}}) ->
  case maps:find(catalog, State) of
    {ok, Catalog} ->
      verify(State#{definition := {ref, Catalog, DefinitionName}});
    error ->
      {error, [no_current_catalog]}
  end;
verify(State = #{options := Options,
                 definition := {ref, Catalog, DefinitionName}}) ->
  case jsv:find_catalog_definition(Catalog, DefinitionName) of
    {ok, Definition} ->
      verify(State#{definition => Definition, catalog => Catalog});
    {error, Reason} ->
      {error, [Reason]}
  end;
verify(#{options := Options,
         definition := {TypeName, Constraints}}) when
    is_atom(TypeName), is_map(Constraints) ->
  case maps:find(TypeName, jsv:type_map(Options)) of
    {ok, Module} ->
      Exported = lists:member({verify_constraint, 2},
                              Module:module_info(exports)),
      VerifyConstraint = case Exported of
                           true ->
                             fun (C, Os) -> Module:verify_constraint(C, Os) end;
                           false ->
                             fun (_, _) -> unknown end
                         end,
      F = fun (ConstraintName, ConstraintValue, Errors) ->
              Constraint = {ConstraintName, ConstraintValue},
              case VerifyConstraint(Constraint, Options) of
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
verify(#{definition := Definition}) ->
  {error, [{invalid_format, Definition}]}.
