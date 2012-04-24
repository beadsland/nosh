-define(VERSION(Module), proplists:get_value(version, Module:module_info(attributes))).

-define(STDERR(Format, List), Stderr ! {self(), stderr, lists:flatten(io_lib:format(Format, List))}).
-define(STDERR(String), ?STDERR(String, [])).
	   
-define(INIT_DEBUG(Pid), put(debug, Pid)).
-define(DEBUG(Format, List), get(debug) ! {self(), debug, lists:flatten(io_lib:format(Format, List))}).
-define(DEBUG(String), ?DEBUG(String, [])).