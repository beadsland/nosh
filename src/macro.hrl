-define(VERSION(Module), proplists:get_value(version, Module:module_info(attributes))).

-define(STDERR(Format, List), Stderr ! {self(), stderr, io_lib:format(Format, List)}).
