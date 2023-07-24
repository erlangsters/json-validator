%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_object_validator_test).
-include_lib("eunit/include/eunit.hrl").

json_object_validator_test() ->
    % Test against all valid JSON-encodable Erlang terms (the scope of this
    % library is to validate valid JSON-encodable Erlang terms which is
    % ensured by the library encoding the raw JSON text).
    Format = {object, [{fields, []}, use_maps]},
    {invalid, not_object} = json_validator:validate(null, Format),
    {invalid, not_object} = json_validator:validate(false, Format),
    {invalid, not_object} = json_validator:validate(true, Format),
    {invalid, not_object} = json_validator:validate(42, Format),
    {invalid, not_object} = json_validator:validate(42.5, Format),
    {invalid, not_object} = json_validator:validate(<<"Hello world!">>, Format),
    {invalid, not_object} = json_validator:validate([], Format),
    valid = json_validator:validate(#{}, Format),

    ok.

json_object_validator_fields_test() ->
    Format = {object, [{fields, [
        {"foo", bool, mandatory},
        {"bar", number, optional}
    ]}, use_maps]},
    Validators = #{
        bool => json_bool_validator,
        number => json_number_validator,
        string => json_string_validator,
        object => json_object_validator
    },

    % Test mandatory fields.
    {invalid, {keys, [{"foo", missing}]}} = term_validator:validate(#{
        <<"bar">> => 42
    }, Format, Validators),

    % Test optional fields.
    valid = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => 42
    }, Format, Validators),

    % Test invalid fields.
    {invalid, {keys, [{"bar", invalid, not_number}]}} = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => true
    }, Format, Validators),

    % Test extra fields.
    {invalid, {keys, [{"quz", unexpected}]}} = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => 42,
        <<"quz">> => <<"Hello world!">>
    }, Format, Validators),

    % Test extra fields.
    {invalid, {keys, [{"quz", unexpected}]}} = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => 42,
        <<"quz">> => <<"Hello world!">>
    }, Format, Validators),

    FormatWithDynamic = {object, [{fields, [
        {"foo", bool, mandatory},
        {"bar", number, optional}
    ]}, dynamic, use_maps]},
    valid = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => 42,
        <<"quz">> => <<"Hello world!">>
    }, FormatWithDynamic, Validators),

    % Test lazy validation.
    {invalid, {keys, Keys}} = term_validator:validate(#{
        <<"bar">> => false,
        <<"quz">> => <<"Hello world!">>
    }, Format, Validators),
    true = lists:member({"foo", missing}, Keys),
    true = lists:member({"bar", invalid, not_number}, Keys),
    true = lists:member({"quz", unexpected}, Keys),

    ok.
