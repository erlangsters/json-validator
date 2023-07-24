%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_null_validator_test).
-include_lib("eunit/include/eunit.hrl").

json_null_validator_test() ->
    % Test against all valid JSON-encodable Erlang terms (the scope of this
    % library is to validate valid JSON-encodable Erlang terms which is
    % ensured by the library encoding the raw JSON text).
    Format = null,
    valid = json_validator:validate(null, Format),
    {invalid, not_null} = json_validator:validate(false, Format),
    {invalid, not_null} = json_validator:validate(true, Format),
    {invalid, not_null} = json_validator:validate(42, Format),
    {invalid, not_null} = json_validator:validate(42.5, Format),
    {invalid, not_null} = json_validator:validate(<<"Hello world!">>, Format),
    {invalid, not_null} = json_validator:validate([], Format),
    {invalid, not_null} = json_validator:validate(#{}, Format),

    ok.

json_null_validator_allow_nill_test() ->
    % Test the 'atom' option (to use a different atom value than 'null').
    Format = {null, [{atom, nill}]},
    Validators = #{null => json_null_validator},

    {invalid, not_null} = term_validator:validate(null, Format, Validators),
    valid = term_validator:validate(nill, Format, Validators),

    ok.
