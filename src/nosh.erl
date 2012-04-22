%% @doc Nosh (nosql shell) Bourne shell emulator and alternative Erlang shell.
%% @author Beads D. Land-Trujillo <beads.d.land@gmail.com>
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: nosh as application
%% TODO: nosh_command behaviour
%% TODO: nosh as nosh_command
%% TODO: Buffered I/O (pending get_char working in noterm)
%% TODO: File System:  pwd / cd
%% TODO: File System:  *NIX / cygwin / Win32
%% TODO: File System:  NoSQL
%% TODO: Env
%% TODO: Sh commands
%% TODO: Erl commands
%% TODO: Couch commands
%% TODO: Add support for line continuation (currently throws error)

%% @version 0.1.0
-module(nosh).
-export([start/1]).
version() -> Version = "0.1.0", Version.


%% @doc Start nosh, passing Pid of process providing standard i/o messaging.
start(Pid) ->
	start(Pid, Pid, Pid).
	
start(Stdin, Stdout, Stderr) ->
	process_flag(trap_exit, true),
	Stdout ! {self(), stdout, io_lib:format("Starting Nosh ~s nosql shell ~p~n", [version(), self()])},

	Dependency = command,
	case code:load_file(Dependency) of 
		{error, Reason} 	-> io:format(standard_error, "~s: ~p~n", [Dependency, Reason]), 
							   init:stop(); 
		{module, _Module} 	-> CmdVersion = Dependency:version(),
							   Stdout ! {self(), stdout, io_lib:format("Using rev. ~s command line parser~n", [CmdVersion])},
							   loop(Stdin, Stdout, Stderr)
	end.	
		
loop(Stdin, Stdout, Stderr) ->
	Stdout ! {self(), stdout, prompt()},
	receive
		{Stdin, stdout, Line}		-> 	Eval = command:eval(Line, Stdin, Stdout, Stderr),
									 	Stdout ! {self(), stdout, io_lib:format("parse: ~p~n", [Eval])};
		{'EXIT', Stdin, Reason}		-> 	io:format("Stopping on terminal exit: ~p ~p~n", [Reason, self()]), 
										init:stop();
		{'EXIT', ExitPid, Reason}	-> 	io:format(standard_error, "** Exit ~p: ~p ~p~n", [ExitPid, Reason, self()]), 
										init:stop()
	end,
	loop(Stdin, Stdout, Stderr).


prompt() ->
	"> ".