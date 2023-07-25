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

json_node_validator_test() ->
    % XXX: To be implemented.

    ok.

json_node_validator_null_term_test() ->
    Validators = json_validator:validators(),

    Format1 = {json, [{format, null}, {null_term, nil}]},
    {invalid, not_null} = term_validator:validate(null, Format1, Validators),
    valid = term_validator:validate(nil, Format1, Validators),

    Format2 = {json, [{format, {array, [{item, null}]}}, {null_term, nil}]},
    {invalid, {items, [{1, not_null}]}} = term_validator:validate([null], Format2, Validators),
    valid = term_validator:validate([nil], Format2, Validators),

    Format3 = {json, [{format, {object, [{fields, [{"foo", null, mandatory}]}]}}, {null_term, nil}, use_maps]},
    {invalid, {keys, [{"foo", invalid, not_null}]}} = term_validator:validate(#{<<"foo">> => null}, Format3, Validators),
    valid = term_validator:validate(#{<<"foo">> => nil}, Format3, Validators),

    ok.

json_node_validator_use_maps_test() ->
    % XXX: To be implemented.
    ok.

