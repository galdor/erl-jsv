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

-module(jsv_generator).

-export([init/3, generate/1, generate_child/3, state_definition/1]).

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
generate(State = #{definition := TypeName}) when is_atom(TypeName) ->
  generate(State#{definition => {TypeName, #{}}});
generate(State = #{definition := {ref, Catalog, DefinitionName}}) ->
  {ok, Definition} = jsv:find_catalog_definition(Catalog, DefinitionName),
  generate(State#{definition => Definition,
                  catalog => Catalog});
generate(State = #{definition := {ref, DefinitionName}}) ->
  case maps:find(catalog, State) of
    {ok, Catalog} ->
      generate(State#{definition => {ref, Catalog, DefinitionName}});
    error ->
      {error, [no_current_catalog]}
  end;
generate(State = #{term := Term,
                   definition := {Type, _},
                   type_map := TypeMap}) ->
  Module = maps:get(Type, TypeMap),
  case lists:member({generate, 2}, Module:module_info(exports)) of
    true ->
      case Module:generate(Term, State) of
        {ok, Value} ->
          {ok, Value};
        invalid ->
          {error, {invalid_value, Term, Type}};
        {error, Reason} ->
          {error, Reason}
      end;
    false ->
      {ok, Term}
  end.

-spec generate_child(term(), jsv:definition(), state()) ->
        {ok, json:value()} | {error, jsv:generation_error_reason()}.
generate_child(Term, Definition, State) ->
  State2 = State#{term => Term, definition => Definition},
  generate(State2).

-spec state_definition(state()) -> jsv:definition().
state_definition(#{definition := Definition}) ->
  Definition.
