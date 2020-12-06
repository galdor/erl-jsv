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

-module(jsv_type_uuid).

-behaviour(jsv_type).

-export([validate_type/1, canonicalize/3, generate/2]).

validate_type(Value) when is_binary(Value) ->
  case uuid:parse(Value) of
    {ok, Id} ->
      {ok, Id};
    {error, _} ->
      error
  end;
validate_type(_) ->
  error.

canonicalize(_, CData, _) ->
  CData.

generate(Term, _) when is_binary(Term), byte_size(Term) =:= 16 ->
  {ok, uuid:format(Term)};
generate(_, _) ->
  error.
