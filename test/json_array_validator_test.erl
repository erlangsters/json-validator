%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_array_validator_test).
-include_lib("eunit/include/eunit.hrl").

json_array_validator_test() ->
    % Test against all valid JSON-encodable Erlang terms (the scope of this
    % library is to validate valid JSON-encodable Erlang terms which is
    % ensured by the library encoding the raw JSON text).
    Format = {array, [{item, any}]},
    {invalid, [], not_array} = json_validator:validate(null, Format),
    {invalid, [], not_array} = json_validator:validate(false, Format),
    {invalid, [], not_array} = json_validator:validate(true, Format),
    {invalid, [], not_array} = json_validator:validate(42, Format),
    {invalid, [], not_array} = json_validator:validate(42.5, Format),
    {invalid, [], not_array} = json_validator:validate(<<"Hello world!">>, Format),
    valid = json_validator:validate([], Format),
    {invalid, [], not_array} = json_validator:validate(#{}, Format),

    ok.

json_array_validator_options_test() ->
    % As it shares the implementation of the 'list' validator of ETV, we do
    % not need to test all its options thoroughly.
    Format = {array, [{item, number}, {length, {4, 6}}]},
    Validators = #{
        array => json_array_validator,
        number => json_number_validator
    },
    {invalid, {length, {must_be_greater_or_equal_to, 4}}} =
        term_validator:validate([1, 2, 3], Format, Validators),
    valid = term_validator:validate([1, 2, 3, 4], Format, Validators),

    valid = term_validator:validate([1, 2, 3, 4, 5, 6], Format, Validators),
    {invalid, {length, {must_be_less_or_equal_to, 6}}} =
        term_validator:validate([1, 2, 3, 4, 5, 6, 7], Format, Validators),

    ok.
