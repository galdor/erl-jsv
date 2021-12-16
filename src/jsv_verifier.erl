%% Copyright (c) 2020-2021 Exograd SAS.
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

-module(jsv_verifier).

-export([init/2, verify/1]).

-export_type([state/0]).

-type state() :: #{options := jsv:options(),
                   definition := jsv:definition(),
                   catalog => jsv:catalog_name(),
                   verified_definitions :=
                     #{{jsv:catalog_name(), jsv:definition_name()} :=
                         boolean()}}.

-spec init(jsv:definition(), jsv:options()) -> state().
init(Definition, Options) ->
  State = #{options => Options,
            definition => Definition,
            verified_definitions => #{}},
  case maps:find(catalog, Options) of
    {ok, Catalog} ->
      State#{catalog => Catalog};
    error ->
      State
  end.

-spec verify(state()) -> ok | {error, [jsv:definition_error_reason()]}.
verify(#{definition := {one_of, []}}) ->
  {error, [invalid_empty_definition_list]};
verify(State = #{definition := {one_of, Definitions}}) when
    is_list(Definitions) ->
  case
    lists:foldl(fun (Def, Acc) ->
                    case verify(State#{definition := Def}) of
                      ok -> Acc;
                      {error, Errors} -> Acc ++ Errors
                    end
                end, [], Definitions)
  of
    [] ->
      ok;
    Errors ->
      {error, Errors}
  end;
verify(State = #{definition := {ref, DefinitionName}}) ->
  case maps:find(catalog, State) of
    {ok, Catalog} ->
      verify(State#{definition := {ref, Catalog, DefinitionName}});
    error ->
      {error, [no_current_catalog]}
  end;
verify(State = #{definition := {ref, Catalog, DefinitionName},
                 verified_definitions := VerifiedDefinitions}) ->
  Key = {Catalog, DefinitionName},
  case maps:is_key(Key, VerifiedDefinitions) of
    true ->
      ok;
    false ->
      case jsv:find_catalog_definition(Catalog, DefinitionName) of
        {ok, Definition} ->
          verify(State#{definition => Definition,
                        catalog => Catalog,
                        verified_definitions =>
                          VerifiedDefinitions#{Key => true}});
        {error, Reason} ->
          {error, [Reason]}
      end
  end;
verify(State = #{definition := TypeName}) when
    is_atom(TypeName) ->
  verify(State#{definition => {TypeName, #{}, #{}}});
verify(State = #{definition := {TypeName, Constraints}}) ->
  verify(State#{definition => {TypeName, Constraints, #{}}});
verify(State = #{options := Options,
                 definition := {TypeName, Constraints, Extra}}) when
    is_atom(TypeName), is_map(Constraints), is_map(Extra) ->
  case maps:find(TypeName, jsv:type_map(Options)) of
    {ok, Module} ->
      F = fun (ConstraintName, ConstraintValue, Errors) ->
              Constraint = {ConstraintName, ConstraintValue},
              Result = case
                         jsv_utils:call_if_defined(Module, verify_constraint,
                                                   [Constraint, State])
                       of
                         {ok, Res} -> Res;
                         undefined -> unknown
                       end,
              case Result of
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
