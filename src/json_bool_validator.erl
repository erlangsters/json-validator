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
-behaviour(term_validator).

-export([mandatory_options/0]).
-export([options/0]).
-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

mandatory_options() -> [].
options() -> [].

pre_validate(Term, _Options, _Validators) ->
    {valid, Term}.

validate(Term, _Option, _Validators) ->
    {valid, Term}.

post_validate(_Term, _Validators) ->
    valid.
