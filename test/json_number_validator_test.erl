%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_number_validator_test).
-include_lib("eunit/include/eunit.hrl").

json_number_validator_test() ->
    % Test against all valid JSON-encodable Erlang terms (the scope of this
    % library is to validate valid JSON-encodable Erlang terms which is
    % ensured by the library encoding the raw JSON text).
    Format = number,
    {invalid, [], not_number} = json_validator:validate(null, Format),
    {invalid, [], not_number} = json_validator:validate(false, Format),
    {invalid, [], not_number} = json_validator:validate(true, Format),
    valid = json_validator:validate(42, Format),
    valid = json_validator:validate(42.5, Format),
    {invalid, [], not_number} = json_validator:validate(<<"Hello world!">>, Format),
    {invalid, [], not_number} = json_validator:validate([], Format),
    {invalid, [], not_number} = json_validator:validate(#{}, Format),

    ok.

json_number_validator_options_test() ->
    % As it shares the implementation of the 'number' validator of ETV, we do
    % not need to test all its options thoroughly.
    Format = {number, [{min, 0}, {max, 10}]},
    Validators = #{number => json_number_validator},

    {invalid, {must_be_greater_or_equal_to, 0}} =
        term_validator:validate(-1, Format, Validators),
    valid = term_validator:validate(0, Format, Validators),
    valid = term_validator:validate(5, Format, Validators),
    valid = term_validator:validate(10, Format, Validators),
    {invalid, {must_be_lower_or_equal_to, 10}} =
        term_validator:validate(11, Format, Validators),

    ok.
