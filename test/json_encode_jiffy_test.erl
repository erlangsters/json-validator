%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_encode_jiffy_test).
-include_lib("eunit/include/eunit.hrl").

%%
%% XXX: More effort could be put into this important unit test that aims at
%%      checking if the entire library works as expected with real-life or
%%      complex input.
%%

valid_json() ->
    <<"
{
    \"yolo\": {
        \"foo\": true,
        \"bar\": 42,
        \"quz\": \"Hello world!\"
    },
    \"oloy\": [null, false, 0]
}
    ">>.

invalid_json() ->
    <<"
{
    \"yolo\": {
        \"foo\": \"Hello world!\",
        \"bar\": 42,
        \"quz\": true
    },
    \"oloy\": [\"null\", \"false\", 0]
}
    ">>.

json_encode_jiffy_test() ->
    Format = {object, [{fields, [
        {"yolo", {object, [{fields, [
            {"foo", bool, mandatory},
            {"bar", number, mandatory},
            {"quz", string, mandatory}
        ]}]}, mandatory},
        {"oloy", {array, [{item, {any_of, [
            null,
            bool,
            number
        ]}}]},
        mandatory}
    ]}]},

    Json = valid_json(),
    Document = jiffy:decode(Json, [return_maps]),
    valid = json_validator:lazy_validate(Document, Format),

    InvalidJson = invalid_json(),
    InvalidDocument = jiffy:decode(InvalidJson, [return_maps]),
    {invalid, Errors} = json_validator:lazy_validate(InvalidDocument, Format),
    true = lists:member({[{key, "yolo"}, {key, "foo"}], not_bool}, Errors),
    true = lists:member({[{key, "yolo"}, {key, "quz"}], not_string}, Errors),

    NotAnyOf = {not_any_of, [null, bool, number]},
    true = lists:member({[{key, "oloy"}, {index, 1}], NotAnyOf}, Errors),
    true = lists:member({[{key, "oloy"}, {index, 2}], NotAnyOf}, Errors),
    false = lists:member({[{key, "oloy"}, {index, 3}], NotAnyOf}, Errors),

    ok.
