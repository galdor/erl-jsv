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

-module(jsv_generator).

-export([init/3, generate/1, generate_child/3,
         state_definition/1, state_options/1]).

-export_type([state/0]).

-opaque state() :: #{options := jsv:options(),
                     term := term(),
                     definition := jsv:definition(),
                     type_map := jsv:type_map(),
                     catalog => jsv:catalog_name()}.

-spec init(term(), jsv:definition(), jsv:options()) -> state().
init(Term, Definition, Options) ->
  TypeMap = maps:get(type_map, Options, jsv:default_type_map()),
  #{options => Options,
    term => Term,
    definition => Definition,
    type_map => TypeMap}.

-spec generate(state()) ->
        {ok, json:value()} | {error, jsv:generation_error_reason()}.
generate(State = #{term := Term, definition := {one_of, Definitions}}) when
    is_list(Definitions) ->
  Fun = fun
          F ([]) ->
            {error, {invalid_value, Term}};
          F ([Def | Defs]) ->
            case generate(State#{definition := Def}) of
              {ok, Value} ->
                {ok, Value};
              {error, _} ->
                F(Defs)
            end
        end,
  Fun(Definitions);
generate(State = #{definition := {ref, DefinitionName}}) ->
  case maps:find(catalog, State) of
    {ok, Catalog} ->
      generate(State#{definition => {ref, Catalog, DefinitionName}});
    error ->
      {error, [no_current_catalog]}
  end;
generate(State = #{definition := {ref, Catalog, DefinitionName}}) ->
  {ok, Definition} = jsv:find_catalog_definition(Catalog, DefinitionName),
  generate(State#{definition => Definition, catalog => Catalog});
generate(State = #{definition := TypeName}) when is_atom(TypeName) ->
  generate(State#{definition => {TypeName, #{}, #{}}});
generate(State = #{definition := {TypeName, Constraints}}) ->
  generate(State#{definition => {TypeName, Constraints, #{}}});
generate(State = #{term := Term,
                   definition := (Definition = {Type, _, _}),
                   type_map := TypeMap}) ->
  case maybe_extra_generate(Term, Definition) of
    {ok, Term2} ->
      Module = maps:get(Type, TypeMap),
      case jsv_utils:call_if_defined(Module, generate, [Term2, State]) of
        {ok, {ok, Value}} ->
          {ok, Value};
        {ok, error} ->
          {error, {invalid_value, Term2, Type}};
        {ok, {error, Reason}} ->
          {error, Reason};
        undefined ->
          {ok, Term}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec maybe_extra_generate(term(), jsv:definition()) ->
        {ok, term()} | {error, jsv:generation_error_reason()}.
maybe_extra_generate(Term, {_, _, #{generate := Generate}}) ->
  case Generate(Term) of
    {ok, Term2} ->
      {ok, Term2};
    {error, Reason} ->
      {error, {invalid_value, Reason}}
  end;
maybe_extra_generate(Term, _) ->
  {ok, Term}.

-spec generate_child(term(), jsv:definition(), state()) ->
        {ok, json:value()} | {error, jsv:generation_error_reason()}.
generate_child(Term, Definition, State) ->
  State2 = State#{term => Term, definition => Definition},
  generate(State2).

-spec state_definition(state()) -> jsv:definition().
state_definition(#{definition := Definition}) ->
  Definition.

-spec state_options(state()) -> jsv:options().
state_options(#{options := Options}) ->
  Options.
