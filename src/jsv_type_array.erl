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

-module(jsv_type_array).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/1, validate_constraint/4, canonicalize/3, generate/2]).

-export_type([constraint/0]).

-type constraint() :: {element, jsv:definition()}
                    | {min_length, non_neg_integer()}
                    | {max_length, non_neg_integer()}.

verify_constraint({element, Definition}, State) ->
  jsv_verifier:verify(State#{definition := Definition});
verify_constraint({min_length, Min}, _) when is_integer(Min), Min >= 0 ->
  ok;
verify_constraint({min_length, _}, _) ->
  invalid;
verify_constraint({max_length, Max}, _) when is_integer(Max), Max >= 0 ->
  ok;
verify_constraint({max_length, _}, _) ->
  invalid;
verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min_length, Min}, _) ->
  {"value must contain at least ~0tp elements", [Min]};

format_constraint_violation({max_length, Max}, _) ->
  {"value must contain at most ~0tp elements", [Max]}.

validate_type(Value) when is_list(Value) ->
  {ok, []};
validate_type(_) ->
  error.

validate_constraint(Value, {element, ElementType}, CData, State) ->
  F = fun (I, Element, {Errors, CData2}) ->
          case
            jsv_validator:validate_child(Element, ElementType,
                                         integer_to_binary(I), State)
          of
            {ok, ElementCData} ->
              {Errors, [ElementCData | CData2]};
            {error, Errors2} ->
              {Errors2 ++ Errors, CData2}
          end
      end,
  case jsv_utils:fold_list_with_index(F, {[], CData}, Value) of
    {[], CData3} ->
      {ok, CData3};
    {Errors, _} ->
      Errors
  end;

validate_constraint(Value, {min_length, Min}, _, _) ->
  length(Value) >= Min;

validate_constraint(Value, {max_length, Max}, _, _) ->
  length(Value) =< Max.

canonicalize(_, CData, _) ->
  lists:reverse(CData).

generate(Term, State) when is_list(Term) ->
  {_, Constraints} = jsv_generator:state_definition(State),
  case Constraints of
    #{element := Definition} ->
      F = fun (Element) ->
              case
                jsv_generator:generate_child(Element, Definition, State)
              of
                {ok, Value} ->
                  Value;
                {error, Reason} ->
                  throw({error, Reason})
              end
          end,
      try
        {ok, lists:map(F, Term)}
      catch
        throw:{error, Reason} ->
          {error, Reason}
      end;
    _ ->
      {ok, Term}
  end;
generate(_, _) ->
  error.
