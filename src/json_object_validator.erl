%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_object_validator).
-behavior(term_validator).

-export([options/1]).

-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

options(optional) -> [dynamic, use_maps];
options(mandatory) -> [fields].

%%
%% JSON object validator.
%%
%% This module implements the JSON object validator.
%%
%% XXX: Update the implementation to re-use the implementation of the built-in
%%      maps validator of ETV.
%%
pre_validate(Term, Options, Validators) when is_map(Term) ->
    % We first compute the missing fields by iterating over the declared
    % mandatory fields and check if they are all present in the Erlang map.
    % Then we compute the invalid and extra fields by iterating over the fields
    % of the Erlang map.
    Fields = proplists:get_value(fields, Options),
    Dynamic = proplists:get_value(dynamic, Options, false),

    Keys0 = lists:foldr(
        fun
            ({Name, _Format, mandatory}, Accumulator) ->
                case maps:is_key(list_to_binary(Name), Term) of
                    true ->
                        Accumulator;
                    false ->
                        [{Name, missing}|Accumulator]
                end;
            ({Name, _Format, optional}, Accumulator) ->
                Accumulator
        end,
        [],
        Fields
    ),
    Keys1 = maps:fold(
        fun(Key, Value, Accumulator) ->
            case proplists:lookup(binary_to_list(Key), Fields) of
                none ->
                    case Dynamic of
                        true ->
                            Accumulator;
                        false ->
                            [{binary_to_list(Key), unexpected}|Accumulator]
                    end;
                {Name, Format, _} ->
                    case term_validator:validate(Value, Format, Validators) of
                        valid ->
                            Accumulator;
                        {invalid, Reason} ->
                            [{Name, invalid, Reason}|Accumulator]
                    end
            end
        end,
        Keys0,
        Term
    ),

    case Keys1 of
        [] ->
            {valid, Term, []};
        _ ->
            {invalid, {keys, Keys1}}
    end;
pre_validate(Term, Options, _Validators) ->
    {invalid, not_object}.

validate(Term, _Option, _Validators) ->
    {valid, Term}.

post_validate(_Term, _Validators) ->
    valid.
