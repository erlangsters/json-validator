%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(json_validator).

-export_type([options/0]).

-export([validate/2, validate/3]).
-export([validators/0]).

%%
%% Main API of the JSON validator.
%%
%% To be written.
%%
-type options() :: [
    use_maps |
    {null_term, term()}
].

-spec validate(term(), term_validator:format()) -> term().
validate(Term, Format) ->
    validate(Term, Format, []).

-spec validate(term(), term_validator:format(), options()) -> term().
validate(Term, Format, _Options) ->
    % XXX: Apply the options.
    NodeFormat = {json, [{format, Format}]},
    term_validator:validate(Term, NodeFormat, validators()).

-spec validators() -> #{atom() => module()}.
validators() ->
    % We also include the 'any', 'any_of' and 'all_of' validators from the
    % Erlang Term Validator library as they are useful in the context of JSON
    % as well.
    #{
        any => any_validator,
        null => json_null_validator,
        bool => json_bool_validator,
        number => json_number_validator,
        string => json_string_validator,
        array => json_array_validator,
        object => json_object_validator,
        any_of => any_of_validator,
        all_of => all_of_validator,
        json => json_node_validator
    }.
