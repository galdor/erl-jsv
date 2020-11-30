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

-module(jsv_type_date).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/1, validate_constraint/4, canonicalize/3,
         format_date/1, is_valid_date/1]).

-export_type([constraint/0]).

-type constraint() :: {min, calendar:date()}
                    | {max, calendar:date()}.

verify_constraint({min, Date = {_, _, _}}, _) ->
  case calendar:valid_date(Date) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({min, _}, _) ->
  invalid;
verify_constraint({max, Date = {_, _, _}}, _) ->
  case calendar:valid_date(Date) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({max, _}, _) ->
  invalid;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min, Min}, _) ->
  {"value must be greater or equal to ~s", [format_date(Min)]};

format_constraint_violation({max, Max}, _) ->
  {"value must be lower or equal to ~s", [format_date(Max)]}.

validate_type(Value) when is_binary(Value) ->
  parse_date(Value);
validate_type(_) ->
  error.

validate_constraint(_, {min, Min}, Date, _) ->
  is_date_after(Date, Min);

validate_constraint(_, {max, Max}, Date, _) ->
  is_date_before(Date, Max).

canonicalize(_, Date, _) ->
  Date.

-spec parse_date(binary()) -> {ok, calendar:date()} | error.
parse_date(Value) ->
  RE = <<"^([0-9]{4})-([0-9]{2})-([0-9]{2})$">>,
  case re:run(Value, RE, [{capture, all_but_first, list}]) of
    {match, [YS, MS, DS]} ->
      try
        Y = list_to_integer(YS),
        M = list_to_integer(MS),
        D = list_to_integer(DS),
        case calendar:valid_date(Y, M, D) of
          true ->
            {ok, {Y, M, D}};
          false ->
            error
        end
      catch
        error:badarg ->
          error
      end;
    nomatch ->
      error
  end.

-spec format_date(calendar:date()) -> binary().
format_date({Y, M, D}) ->
  iolist_to_binary(io_lib:format(<<"~4..0b-~2..0b-~2..0b">>, [Y, M, D])).

-spec is_valid_date(calendar:date()) -> boolean().
is_valid_date({Y, M, D}) when
    is_integer(Y), is_integer(M), is_integer(D) ->
  calendar:valid_date(Y, M, D);
is_valid_date(_) ->
  false.

-spec is_date_after(calendar:date(), calendar:date()) -> boolean().
is_date_after(D1, D2) ->
  calendar:date_to_gregorian_days(D1) >= calendar:date_to_gregorian_days(D2).

-spec is_date_before(calendar:date(), calendar:date()) -> boolean().
is_date_before(D1, D2) ->
  calendar:date_to_gregorian_days(D1) =< calendar:date_to_gregorian_days(D2).
