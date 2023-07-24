%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(json_null_validator).
-behavior(term_validator).

-export([options/1]).

-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

%%
%% JSON null validator.
%%
%% This module implements the JSON null validator. By default, it uses the
%% 'null' atom but it can be configured to use a different atom value (e.g.
%% 'nil') by setting the 'atom' option.
%%

options(optional) -> [atom];
options(mandatory) -> [].

pre_validate(Value, [{atom, Value}], _Validators) ->
    {valid, null, []};
pre_validate(_Value1, [{atom, _Value2}], _Validators) ->
    {invalid, not_null};
pre_validate(null, _Options, _Validators) ->
    {valid, null, []};
pre_validate(_Term, _Options, _Validators) ->
    {invalid, not_null}.

validate(Term, _Option, _Validators) ->
    {valid, Term}.

post_validate(_Term, _Validators) ->
    valid.
