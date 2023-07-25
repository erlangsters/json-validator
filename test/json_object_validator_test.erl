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
    {invalid, [], not_object} = json_validator:validate(null, Format),
    {invalid, [], not_object} = json_validator:validate(false, Format),
    {invalid, [], not_object} = json_validator:validate(true, Format),
    {invalid, [], not_object} = json_validator:validate(42, Format),
    {invalid, [], not_object} = json_validator:validate(42.5, Format),
    {invalid, [], not_object} = json_validator:validate(<<"Hello world!">>, Format),
    {invalid, [], not_object} = json_validator:validate([], Format),
    valid = json_validator:validate(#{}, Format),

    % XXX: Test with and without the 'use_maps' option as global options
    %      (for now implementation always default to using maps).

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
    {invalid, {missing_fields, ["foo"]}} = term_validator:validate(#{
        <<"bar">> => 42
    }, Format, Validators),

    % Test optional fields.
    valid = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => 42
    }, Format, Validators),

    % Test invalid fields.
    {invalid, {fields, [{"bar", not_number}]}} = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => true
    }, Format, Validators),

    % Test extra fields.
    {invalid, {unexpected_fields, ["quz"]}} = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => 42,
        <<"quz">> => <<"Hello world!">>
    }, Format, Validators),

    % Test lazy validation.
    {invalid, {fields, Fields}} = term_validator:validate(#{
        <<"foo">> => 42,
        <<"bar">> => <<"Hello world!">>
    }, Format, Validators),
    true = lists:member({"foo", not_bool}, Fields),
    true = lists:member({"bar", not_number}, Fields),

    ok.

json_object_validator_dynamic_test() ->
    % Same format as above test, but with the "dynamic" option enabled.
    Format = {object, [{fields, [
        {"foo", bool, mandatory},
        {"bar", number, optional}
    ]}, dynamic, use_maps]},
    Validators = #{
        bool => json_bool_validator,
        number => json_number_validator,
        string => json_string_validator,
        object => json_object_validator
    },

    valid = term_validator:validate(#{
        <<"foo">> => false,
        <<"bar">> => 42,
        <<"quz">> => <<"Hello world!">>
    }, Format, Validators),

    ok.
