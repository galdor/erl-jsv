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

-module(jsv_utils).

-export([fold_list_with_index/3,
         call_if_defined/3,
         remove_null_map_entries/1]).

-spec fold_list_with_index(fun((Index, Element, Acc) -> Acc), Acc, [Element]) ->
        Acc when
    Index :: non_neg_integer(),
    Element :: any(),
    Acc :: any().
fold_list_with_index(F, Acc, List) ->
  {_, Acc2} = lists:foldl(fun (Element, {I, Acc2}) ->
                              {I+1, F(I, Element, Acc2)}
                          end, {0, Acc}, List),
  Acc2.

-spec call_if_defined(module(), atom(), [term()]) -> {ok, term()} | undefined.
call_if_defined(Module, Function, Args) ->
  %% Checking if a module exports a function is surprisingly tricky:
  %% erlang:function_exported/3 will always return false when running in an
  %% escript, making it useless. Looking up the content of
  %% Module:module_info(exports) works, but is incredibly slow; slow enough
  %% that it becomes a huge bottleneck during verification, validation and
  %% generation.
  %%
  %% And of course, both of them may introduce a race condition since the
  %% module being checked can be upgraded after the verification but before
  %% the actual call.
  %%
  %% Ultimately, the only fast and accurate way is to check for undef.
  try
    {ok, apply(Module, Function, Args)}
  catch
    error:undef ->
      undefined
  end.

-spec remove_null_map_entries(map()) -> map().
remove_null_map_entries(Map) ->
  maps:filter(fun (_, V) -> V =/= null end, Map).
