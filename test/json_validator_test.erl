%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(json_validator_test).
-include_lib("eunit/include/eunit.hrl").

json_validator_test() ->
    valid = json_validator:validate(null, null),
    valid = json_validator:validate(null, {null, []}),

    valid = json_validator:validate(false, bool),
    valid = json_validator:validate(false, {bool, []}),
    valid = json_validator:validate(true, bool),
    valid = json_validator:validate(true, {bool, []}),

    valid = json_validator:validate(42, number),
    valid = json_validator:validate(42, {number, []}),
    valid = json_validator:validate(42.5, number),
    valid = json_validator:validate(42.5, {number, []}),

    valid = json_validator:validate(<<"Hello world!">>, string),
    valid = json_validator:validate(<<"Hello world!">>, {string, []}),

    valid = json_validator:validate([], {array, [{item, any}]}),
    valid = json_validator:validate(#{}, {object, [{fields, []}]}),

    ok.
