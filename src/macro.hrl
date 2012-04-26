-import(proplists).
-define(ATTRIB(Module, Attribute), proplists:get_value(Attribute, Module:module_info(attributes))).
-define(VERSION(Module), ?ATTRIB(Module, version)).

-import(io_lib).
-import(lists).
-define(STDERR(Format, List), Stderr ! {self(), stderr, lists:flatten(io_lib:format(Format, List))}).
-define(STDERR(String), ?STDERR(String, [])).

-define(INIT_DEBUG(Pid), put(debug, Pid)).

-ifdef(debug).
-define(DEBUG(Format, List), get(debug) ! {self(), debug, lists:flatten(io_lib:format(Format, List))}).
-else.
-define(DEBUG(F, L), self() ! {ignore, F, L}).
-endif.
-define(DEBUG(String), ?DEBUG(String, [])).  