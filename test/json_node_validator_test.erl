%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(json_node_validator_test).
-include_lib("eunit/include/eunit.hrl").

json_node_validator_errors_test() ->
    InvalidTerm = spawn(fun() -> ok end),
    Validators = maps:merge(
        json_validator:validators(),
        #{node => json_node_validator}
    ),

    {invalid, [{[], not_null}]} = term_validator:validate(
        InvalidTerm,
        {node, [{format, null}]},
        Validators
    ),

    {invalid, [{[], not_bool}]} = term_validator:validate(
        InvalidTerm,
        {node, [{format, bool}]},
        Validators
    ),

    {invalid, [{[], not_number}]} = term_validator:validate(
        InvalidTerm,
        {node, [{format, number}]},
        Validators
    ),

    {invalid, [{[], not_string}]} = term_validator:validate(
        InvalidTerm,
        {node, [{format, string}]},
        Validators
    ),

    {invalid, [{[], not_array}]} = term_validator:validate(
        InvalidTerm,
        {node, [{format, {array, [{item, any}]}}]},
        Validators
    ),

    {invalid, [
        {[{index, 3}], not_null},
        {[{index, 2}], not_null},
        {[{index, 1}], not_null}
    ]} = term_validator:validate(
        [InvalidTerm, InvalidTerm, InvalidTerm],
        {node, [{format, {array, [{item, null}]}}]},
        Validators
    ),

    {invalid, [
        {[{key, "bar"}], not_null},
        {[{key, "foo"}], not_null},
        {[{key, "quz"}], not_null}
    ]} = term_validator:validate(
        #{
            <<"foo">> => InvalidTerm,
            <<"bar">> => InvalidTerm,
            <<"quz">> => InvalidTerm
        },
        {node, [{format, {object, [{fields, [
            {"foo", null, mandatory},
            {"bar", null, mandatory},
            {"quz", null, mandatory}
        ]}]}}]},
        Validators
    ),

    ok.

json_node_validator_null_term_test() ->
    Validators = maps:merge(
        json_validator:validators(),
        #{node => json_node_validator}
    ),

    Format1 = {node, [{format, null}, {null_term, nil}]},
    {invalid, [{[], not_null}]} = term_validator:validate(null, Format1, Validators),
    valid = term_validator:validate(nil, Format1, Validators),

    Format2 = {node, [{format, {array, [{item, null}]}}, {null_term, nil}]},
    {invalid, [{[{index, 1}], not_null}]} = term_validator:validate([null], Format2, Validators),
    valid = term_validator:validate([nil], Format2, Validators),

    Format3 = {node, [{format, {object, [{fields, [{"foo", null, mandatory}]}]}}, {null_term, nil}, use_maps]},
    {invalid, [{[{key, "foo"}], not_null}]} = term_validator:validate(#{<<"foo">> => null}, Format3, Validators),
    valid = term_validator:validate(#{<<"foo">> => nil}, Format3, Validators),

    ok.

json_node_validator_use_maps_test() ->
    % XXX: To be implemented.
    ok.
