%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_bool_validator_test).
-include_lib("eunit/include/eunit.hrl").

json_bool_validator_test() ->
    % Test against all valid JSON-encodable Erlang terms (the scope of this
    % library is to validate valid JSON-encodable Erlang terms which is
    % ensured by the library encoding the raw JSON text).
    Format = bool,
    {invalid, [], not_bool} = json_validator:validate(null, Format),
    valid = json_validator:validate(false, Format),
    valid = json_validator:validate(true, Format),
    {invalid, [], not_bool} = json_validator:validate(42, Format),
    {invalid, [], not_bool} = json_validator:validate(42.5, Format),
    {invalid, [], not_bool} = json_validator:validate(<<"Hello world!">>, Format),
    {invalid, [], not_bool} = json_validator:validate([], Format),
    {invalid, [], not_bool} = json_validator:validate(#{}, Format),

    ok.
