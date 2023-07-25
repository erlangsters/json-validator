%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the JSON Validator project from Erlangsters and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(json_node_validator).
-behavior(term_validator).

-export([options/1]).

-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

%%
%% JSON node validator.
%%
%% To be written.
%%

options(optional) -> [null_term, use_maps];
options(mandatory) -> [format].

pre_validate(Term, Options, _Validators) ->
    % We must update the format of the root node with the global options,
    % recursively.
    Format = proplists:get_value(format, Options),

    NullTerm = proplists:get_value(null_term, Options, null),
    UseMaps = proplists:get_value(use_maps, Options, false),

    AdjustedFormat = adjust_format(
        Format,
        #{null_term => NullTerm, use_maps => UseMaps}
    ),

    % We adjust the options to use the adjusted format instead, and we also
    % remove the global options that are already applied.
    {valid, Term, [{format, AdjustedFormat}]}.

validate(Term, {format, Format}, Validators) ->
    % We remove this validator from the list of validators as it's just used
    % to bootstrap the validation.
    UpdatedValidators = maps:remove(node, Validators),

    case term_validator:validate(Term, Format, UpdatedValidators) of
        valid ->
            {valid, Term};
        {invalid, Reason} ->
            {invalid, compute_errors([], Format, Reason)}
    end.

post_validate(_Term, _Validators) ->
    valid.

compute_errors(Path, Format, Reason) when is_atom(Format) ->
    % Normalize the format to be in its tuple form.
    compute_errors(Path, {Format, []}, Reason);
compute_errors(Path, {array, Options}, {items, InvalidItems}) ->
    % If the node format is a JSON array and the invalid reason is because some
    % items are invalid, then we compute the path for each invalid item.
    ItemFormat = proplists:get_value(item, Options),
    lists:foldr(
        fun({Index, Reason}, Accumulator) ->
            Accumulator ++ compute_errors(Path ++ [{index, Index}], ItemFormat, Reason)
        end,
        [],
        InvalidItems
    );
compute_errors(Path, {object, Options}, {fields, InvalidFields}) ->
    % If the node format is a JSON object and the invalid reason is because
    % some fields are invalid, then we compute the path for each invalid field.
    FieldsFormat = proplists:get_value(fields, Options),
    lists:foldr(fun({Key, Reason}, Accumulator) ->
        {Key, FieldFormat, _} = proplists:lookup(Key, FieldsFormat),
        Accumulator ++ compute_errors(Path ++ [{key, Key}], FieldFormat, Reason)
    end, [], InvalidFields);
compute_errors(Path, _Format, Reason) ->
    % The other reasons do not contain embedded reasons.
    [{Path, Reason}].

adjust_format(Format, Options) when is_atom(Format) ->
    % Normalize the format to be in its tuple form.
    adjust_format({Format, []}, Options);
adjust_format({null, [{atom, _}]}, Options) ->
    % The 'null_term' should not be set on a per node basis (it should be set
    % using the global options); we override it.
    adjust_format({null, []}, Options);
adjust_format({null, []}, #{null_term := NullTerm}) ->
    % The JSON null formats needs to be adjusted to use the 'null_term' option.
    {null, [{atom, NullTerm}]};
adjust_format({array, Options}, GlobalOptions) ->
    % The JSON array formats needs to be adjusted with the adjusted nested node
    % format.
    ItemFormat = proplists:get_value(item, Options),
    AdjustedOptions = lists:keyreplace(
        item,
        1,
        Options,
        {item, adjust_format(ItemFormat, GlobalOptions)}
    ),
    {array, AdjustedOptions};
adjust_format({object, Options}, #{use_maps := _UseMaps} = GlobalOptions) ->
    % XXX: Update this node options to use the global options (UseMaps is not
    %      used).

    % The JSON object formats needs to be adjusted to use the 'use_maps' option
    % and with the adjusted nested node formats.
    case proplists:get_value(use_maps, Options, false) of
        true ->
            % The 'use_maps' should not be est on a per node basis (it should
            % be set using the global options); we override it.
            adjust_format(
                {object, proplists:delete(use_maps, Options)},
                GlobalOptions
            );
        false ->
            FieldsFormat = proplists:get_value(fields, Options),
            AdjustedFieldsFormat = lists:map(
                fun({Name, Format, Optionality}) ->
                    {Name, adjust_format(Format, GlobalOptions), Optionality}
                end,
                FieldsFormat
            ),
            AdjustedOptions = lists:keyreplace(
                fields,
                1,
                Options,
                {fields, AdjustedFieldsFormat}
            ),
            {object, AdjustedOptions}
    end;
adjust_format(Format, _Options) ->
    % The other formats are not modified.
    Format.
