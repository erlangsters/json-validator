{application, 'json_validator', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['json_array_validator','json_bool_validator','json_node_validator','json_null_validator','json_number_validator','json_object_validator','json_string_validator','json_validator']},
	{registered, []},
	{applications, [kernel,stdlib,etv]},
	{env, []}
]}.