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

-module(jsv_type_time).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/1, validate_constraint/3,
         format_time/1, is_valid_time/1]).

-export_type([constraint/0]).

-type constraint() :: {min, calendar:time()}
                    | {max, calendar:time()}.

verify_constraint({min, Min}, _) ->
  case is_valid_time(Min) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({max, Max}, _) ->
  case is_valid_time(Max) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min, Min}, _) ->
  {"value must be greater or equal to ~s", [format_time(Min)]};

format_constraint_violation({max, Max}, _) ->
  {"value must be lower or equal to ~s", [format_time(Max)]}.

validate_type(Value) when is_binary(Value) ->
  parse_time(Value);
validate_type(_) ->
  error.

validate_constraint(Value, {min, Min}, _) ->
  is_time_after(Value, Min);

validate_constraint(Value, {max, Max}, _) ->
  is_time_before(Value, Max).

-spec parse_time(binary()) -> {ok, calendar:time()} | error.
parse_time(Value) ->
  RE = <<"^([0-9]{2}):([0-9]{2}):([0-9]{2})$">>,
  case re:run(Value, RE, [{capture, all_but_first, list}]) of
    {match, [HS, MS, SS]} ->
      try
        H = list_to_integer(HS),
        M = list_to_integer(MS),
        S = list_to_integer(SS),
        if
          H >= 0, H < 24, M >= 0, M < 60, S >= 0, S =< 60 ->
            {ok, {H, M, S}};
          true ->
            error
        end
      catch
        error:badarg ->
          error
      end;
    nomatch ->
      error
  end.

-spec format_time(calendar:time()) -> binary().
format_time({H, M, S}) ->
  iolist_to_binary(io_lib:format(<<"~2..0b:~2..0b:~2..0b">>, [H, M, S])).

-spec is_valid_time(calendar:time()) -> boolean().
is_valid_time({H, M, S}) when
    is_integer(H), is_integer(M), is_integer(S),
    H >= 0, H < 24, M >= 0, M < 60, S >= 0, S =< 60 ->
  true;
is_valid_time(_) ->
  false.

-spec is_time_after(calendar:time(), calendar:time()) -> boolean().
is_time_after(T1, T2) ->
  calendar:time_to_seconds(T1) >= calendar:time_to_seconds(T2).

-spec is_time_before(calendar:time(), calendar:time()) -> boolean().
is_time_before(T1, T2) ->
  calendar:time_to_seconds(T1) =< calendar:time_to_seconds(T2).
