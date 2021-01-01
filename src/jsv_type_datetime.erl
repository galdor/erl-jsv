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

-module(jsv_type_datetime).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/1, validate_constraint/4, canonicalize/3, generate/2,
         format_datetime/1, is_valid_datetime/1]).

-export_type([constraint/0]).

-type constraint() :: {min, calendar:datetime()}
                    | {max, calendar:datetime()}.

verify_constraint({min, Min}, _) ->
  case is_valid_datetime(Min) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({max, Max}, _) ->
  case is_valid_datetime(Max) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min, Min}, _) ->
  {"value must be greater or equal to ~s", [format_datetime(Min)]};

format_constraint_violation({max, Max}, _) ->
  {"value must be lower or equal to ~s", [format_datetime(Max)]}.

validate_type(Value) when is_binary(Value) ->
  try
    String = binary_to_list(Value),
    {ok, calendar:rfc3339_to_system_time(String)}
  catch
    error:_ ->
      error
  end;
validate_type(_) ->
  error.

validate_constraint(_, {min, Min}, SystemTime, _) ->
  is_datetime_after(SystemTime, Min);

validate_constraint(_, {max, Max}, SystemTime, _) ->
  is_datetime_before(SystemTime, Max).

canonicalize(_, SystemTime, _) ->
  system_time_to_datetime(SystemTime).

generate(Datetime, _) ->
  case is_valid_datetime(Datetime) of
    true ->
      {ok, format_datetime(Datetime)};
    false ->
      error
  end.

-spec format_datetime(calendar:datetime()) -> binary().
format_datetime({Date, Time}) ->
  Data = [jsv_type_date:format_date(Date),
          $T, jsv_type_time:format_time(Time) ,$Z],
  iolist_to_binary(Data).

-spec is_valid_datetime(calendar:datetime()) -> boolean().
is_valid_datetime({Date, Time}) ->
  jsv_type_date:is_valid_date(Date) and jsv_type_time:is_valid_time(Time);
is_valid_datetime(_) ->
  false.

-spec datetime_to_system_time(calendar:datetime()) -> integer().
datetime_to_system_time(DT) ->
  Seconds = calendar:datetime_to_gregorian_seconds(DT),
  Offset = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Seconds - Offset.

-spec system_time_to_datetime(integer()) -> calendar:datetime().
system_time_to_datetime(SystemTime) ->
  Offset = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Seconds = SystemTime + Offset,
  calendar:gregorian_seconds_to_datetime(Seconds).

-spec is_datetime_after(SystemTime :: integer(), calendar:datetime()) ->
        boolean().
is_datetime_after(SysTime1, DT2) ->
  SysTime2 = datetime_to_system_time(DT2),
  SysTime1 >= SysTime2.

-spec is_datetime_before(SystemTime :: integer(), calendar:datetime()) ->
        boolean().
is_datetime_before(SysTime1, DT2) ->
  SysTime2 = datetime_to_system_time(DT2),
  SysTime1 =< SysTime2.
