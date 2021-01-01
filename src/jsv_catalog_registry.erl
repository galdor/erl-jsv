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

-module(jsv_catalog_registry).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([table_name/1, start_link/0, register_catalog/2, unregister_catalog/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-type state() :: #{tables := #{jsv:catalog_name() := ets:tid()}}.

-spec table_name(jsv:catalog_name()) -> jsv:catalog_table_name().
table_name(Name) ->
  Bin = <<"jsv_catalog_", (atom_to_binary(Name))/binary>>,
  binary_to_atom(Bin).

-spec start_link() -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_catalog(jsv:catalog_name(), jsv:catalog()) ->
        jsv:catalog_table_name().
register_catalog(Name, Catalog) ->
  gen_server:call(?MODULE, {register_catalog, Name, Catalog}).

-spec unregister_catalog(jsv:catalog_name()) -> ok.
unregister_catalog(Name) ->
  gen_server:call(?MODULE, {unregister_catalog, Name}).

-spec init(list()) -> {ok, state()}.
init([]) ->
  logger:update_process_metadata(#{domain => [jsv, catalog_registry]}),
  State = #{tables => #{}},
  {ok, State}.

terminate(_Reason, #{tables := Tables}) ->
  lists:foreach(fun ({_, Id}) -> ets:delete(Id) end,
                maps:to_list(Tables)),
  ok.

handle_call({register_catalog, Name, Catalog}, _From, State) ->
  {TableName, State2} = do_register_catalog(Name, Catalog, State),
  {reply, TableName, State2};

handle_call({unregister_catalog, Name}, _From, State) ->
  State2 = do_unregister_catalog(Name, State),
  {reply, ok, State2};

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec do_register_catalog(jsv:catalog_name(), jsv:catalog(), state()) ->
        {jsv:catalog_table_name(), state()}.
do_register_catalog(Name, Catalog, State = #{tables := Tables}) ->
  TableName = table_name(Name),
  Id = ets:new(TableName, [set,
                           named_table,
                           {read_concurrency, true}]),
  lists:foreach(fun (Pair) ->
                    ets:insert(TableName, Pair)
                end, maps:to_list(Catalog)),
  {Name, State#{tables => Tables#{Name => Id}}}.

-spec do_unregister_catalog(jsv:catalog_name(), state()) -> state().
do_unregister_catalog(Name, State = #{tables := Tables}) ->
  TableName = table_name(Name),
  ets:delete(TableName),
  State#{tables => maps:remove(Name, Tables)}.
