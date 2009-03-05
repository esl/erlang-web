-define(INVALIDATE(Regexps), 
	-invalidate(Regexps)).

-define(INVALIDATE_IF(Regexps, Pred),
	-invalidate(Regexps, Pred)).

-define(INVALIDATE_GROUPS(Groups),
	-invalidate_groups(Groups)).

-define(INVALIDATE_GROUPS_IF(Groups, Pred),
	-invalidate_groups(Groups, Pred)).

-define(BACKEND_CALL,
	-backend_call([])).
