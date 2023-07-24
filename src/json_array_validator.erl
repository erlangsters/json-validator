%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_array_validator).
-behavior(term_validator).

-export([options/1]).

-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

%%
%% JSON array validator.
%%
%% This module implements the JSON array validator. It re-uses the
%% implementation of the built-in list validator of ETV.
%%

options(optional) ->
    list_validator:options(optional);
options(mandatory) ->
    list_validator:options(mandatory).

pre_validate(Term, Options, Validators) ->
    case list_validator:pre_validate(Term, Options, Validators) of
        {invalid, not_list} ->
            {invalid, not_array};
        Any ->
            Any
    end.

validate(Term, Option, Validators) ->
    list_validator:validate(Term, Option, Validators).

post_validate(Term, Validators) ->
    list_validator:post_validate(Term, Validators).
