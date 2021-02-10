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

-module(jsv_type_email_address).

-behaviour(jsv_type).

-export([validate_type/2, generate/2]).

validate_type(Value, _) when is_binary(Value) ->
  %% We use a minimal regular expression on purpose: real email validation is
  %% incredibly complex and it always ends up being a trade off between
  %% supporting obscure formats and rejecting common user mistakes.
  %%
  %% Anyone using email addresses will have to implement another layer of
  %% validation (MX check, etc.) anyway.
  case re:run(Value, "^[^@]+@.+$") of
    {match, _} ->
      ok;
    nomatch ->
      error
  end;
validate_type(_, _) ->
  error.

generate(Term, _) when is_binary(Term) ->
  {ok, Term};
generate(_, _) ->
  error.
