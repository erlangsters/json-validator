%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_number_validator).
-behaviour(term_validator).

-export([options/1]).

-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

%%
%% JSON number validator.
%%
%% This module implements the JSON number validator. It re-uses the
%% implementation of the built-in number validator of ETV.
%%

options(optional) ->
    number_validator:options(optional);
options(mandatory) ->
    number_validator:options(mandatory).

pre_validate(Term, Options, Validators) ->
    number_validator:pre_validate(Term, Options, Validators).

validate(Term, Option, Validators) ->
    number_validator:validate(Term, Option, Validators).

post_validate(Term, Validators) ->
    number_validator:post_validate(Term, Validators).
