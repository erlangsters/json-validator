%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_bool_validator).
-behavior(term_validator).

-export([options/1]).

-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

%%
%% JSON boolean validator.
%%
%% This module implements the JSON boolean validator. It only accepts the
%% 'true' and 'false' atoms.
%%

options(optional) -> [];
options(mandatory) -> [].

pre_validate(true, _Options, _Validators) ->
    {valid, true, []};
pre_validate(false, _Options, _Validators) ->
    {valid, false, []};
pre_validate(_Term, _Options, _Validators) ->
    {invalid, not_bool}.

validate(Term, _Option, _Validators) ->
    {valid, Term}.

post_validate(_Term, _Validators) ->
    valid.
