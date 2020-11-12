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

-module(jsv_type_object).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/1, validate_constraint/3]).

-export_type([constraint/0]).

-type constraint() :: {value_type, jsv:definition()}
                    | {min_size, non_neg_integer()}
                    | {max_size, non_neg_integer()}
                    | {required, Names :: [jsv:keyword()]}
                    | {members, #{jsv:keyword() := jsv:definition()}}.

verify_constraint({value_type, Definition}, State) ->
  jsv:verify_definition(Definition, State);

verify_constraint({min_size, Min}, _) when is_integer(Min), Min >= 0 ->
  ok;
verify_constraint({min_size, _}, _) ->
  invalid;

verify_constraint({max_size, Max}, _) when is_integer(Max), Max >= 0 ->
  ok;
verify_constraint({max_size, _}, _) ->
  invalid;

verify_constraint({required, Names}, _) when is_list(Names) ->
  case lists:all(fun jsv:is_keyword/1, Names) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({required, _}, _) ->
  invalid;

verify_constraint({members, Definitions}, State) when is_map(Definitions) ->
  case lists:all(fun jsv:is_keyword/1, maps:keys(Definitions)) of
    true ->
      F = fun (_, Definition, Errors) ->
              case jsv:verify_definition(Definition, State) of
                ok ->
                  Errors;
                {error, Errors2} ->
                  Errors2 ++ Errors
              end
          end,
      case maps:fold(F, [], Definitions) of
        [] ->
          ok;
        Errors ->
          {error, Errors}
      end;
    false ->
      invalid
  end;
verify_constraint({members, _}, _) ->
  invalid;

verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min_size, Min}, _) ->
  {"value must contain at least ~0tp members", [Min]};

format_constraint_violation({max_size, Max}, _) ->
  {"value must contain at most ~0tp members", [Max]};

format_constraint_violation({required, _}, {missing_names, [Name]}) ->
  {"value must contain the following member: ~ts", [Name]};
format_constraint_violation({required, _}, {missing_names, Names}) ->
  Data = lists:join(<<", ">>, lists:map(fun jsv:keyword_value/1, Names)),
  {"value must contain the following members: ~ts", [iolist_to_binary(Data)]}.

validate_type(Value) when is_map(Value) ->
  ok;
validate_type(_) ->
  error.

validate_constraint(Value, {value_type, ValueType}, State) ->
  F = fun (MemberName, MemberValue, Errors) ->
          case
            jsv_validator:validate_child(MemberValue, ValueType, MemberName,
                                         State)
          of
            ok ->
              Errors;
            {error, Errors2} ->
              Errors2 ++ Errors
          end
      end,
  maps:fold(F, [], Value);

validate_constraint(Value, {min_size, Min}, _) ->
  map_size(Value) >= Min;

validate_constraint(Value, {max_size, Max}, _) ->
  map_size(Value) =< Max;

validate_constraint(Value, {required, Names}, _) ->
  F = fun (Name0, MissingNames) ->
          Name = jsv:keyword_value(Name0),
          case maps:is_key(Name, Value) of
            true ->
              MissingNames;
            false ->
              [Name | MissingNames]
          end
      end,
  case lists:foldl(F, [], Names) of
    [] ->
      ok;
    MissingNames ->
      {invalid, {missing_names, lists:reverse(MissingNames)}}
  end;

validate_constraint(Value, {members, Definitions}, State) ->
  F = fun (MemberName0, Definition, Errors) ->
          MemberName = jsv:keyword_value(MemberName0),
          case maps:find(MemberName, Value) of
            {ok, MemberValue} ->
              case
                jsv_validator:validate_child(MemberValue, Definition,
                                             MemberName, State)
              of
                ok ->
                  Errors;
                {error, Errors2} ->
                  Errors2 ++ Errors
              end;
            error ->
              Errors
          end
      end,
  maps:fold(F, [], Definitions).
