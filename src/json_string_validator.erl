%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_string_validator).
-behaviour(term_validator).

-export([options/1]).

-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

%%
%% JSON string validator.
%%
%% This module implements the JSON string validator. We want the same
%% interface as the built-in string validator of ETV, so we re-use its
%% implementation for the most part. The only difference is that we expect the
%% string value in binary format. Therefore we convert is first then pass it
%% to the implementation of the built-in string validator.
%%

options(optional) ->
    string_validator:options(optional);
options(mandatory) ->
    string_validator:options(mandatory).

pre_validate(Term, Options, Validators) when is_binary(Term) ->
    string_validator:pre_validate(
        unicode:characters_to_list(Term),
        Options,
        Validators
    );
pre_validate(_Term, _Options, _Validators) ->
    {invalid, not_string}.

validate(Term, Option, Validators) ->
    string_validator:validate(Term, Option, Validators).

post_validate(Term, Validators) ->
    string_validator:post_validate(Term, Validators).
