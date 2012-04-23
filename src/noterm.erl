%% @doc Terminal emulator for <code>nosh</code>.
%% Translating standard I/O to Erlang messaging.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: io:get_char (see code in jungerl)
%% TODO: escript and parameters (make it run like any other shell command)
%% TODO: make fully redistributable (Win/cygwin/*NIX)
%% TODO: incorporate full terminfo/ncurses support
%% TODO: notermd - telent/ssh access

%% @version 0.1.1
-module(noterm).
-export([start/0]).
-export([key_start/1]).
version() -> Version = "0.1.1", Version. 


%% @doc Start terminal, launching message loop and keyboard listening process. 
start() ->
	process_flag(trap_exit, true),
	io:format("Starting Noterm ~s terminal emulator on ~p ~p~n", [version(), node(), self()]),
	
	Dependency = nosh,
	case code:load_file(Dependency) of 
		{error, Reason} 	-> io:format(standard_error, "~s: ~p~n", [Dependency, Reason]), init:stop(); 
		{module, _Module} 	-> loop_start(Dependency)
	end,
	{ok, self()}.

loop_start(Shell) ->
	NoshPid = spawn_link(Shell, start, [self()]), 
	KeyPid = spawn_link(?MODULE, key_start, [self()]),
	msg_loop(KeyPid, NoshPid, NoshPid).	
							   
msg_loop(Stdin, Stdout, Stderr) ->
	receive
		{Stdin, stdout, Line}		-> Stdout ! {self(), stdout, strip_escapes(Line)};
		{Stdin, stderr, Line}		-> io:format(standard_error, "** ~s", [Line]); % key err doesn't go to shell
		{Stdout, stdout, Line} 		-> io:format(Line, []);
		{Stderr, stderr, Line} 		-> io:format(standard_error, "** ~s", [Line]);
		{'EXIT', Stdin, Reason}  	-> grace("Stopping on keyboard exit", Reason);
		{'EXIT', Stdout, Reason}	-> grace("Stopping on shell exit", Reason), init:stop();
		{'EXIT', ExitPid, Reason}	-> grace(io_lib:format("Stopping on ~p exit", [ExitPid]), Reason)
    end,
	msg_loop(Stdin, Stdout, Stderr). 

grace(Message, Reason) -> 
	case Reason of
		{{Exception, ExcReason}, Trace} 	-> 
			Format = "~s: ~p ~p~nContext: ~p~nTrace: ~p~n",
			io:format(standard_error, Format, [Message, Exception, self(), ExcReason, Trace]),
			init:stop(); 
		Else						->
			io:format(standard_error, "~s: ~p ~p~n", [Message, Else, self()])
	end,
	exit(normal).

strip_escapes(Subject) ->
	{ok, MP} = re:compile("\e\[[\d,\s]+[A-Z]"),
	re:replace(Subject, MP, "", [global, {return, list}]).

%%========================================
%% Functions for keyboard process.
%%========================================

%%@private Export to allow for spawn.
key_start(Pid) -> 
	Pid ! {self(), stderr, io_lib:format("Listening to keyboard ~p~n", [self()])},
	key_loop(Pid, Pid, Pid). 

key_loop(Stdin, Stdout, Stderr) ->
	case io:get_line("") of 
		ok			    ->  receive
								{'EXIT', Stdin, Reason} -> io:format("~s exit: ~s~n", [?MODULE, Reason])
							after 
								1 -> false		
							end;
		eof 			->	key_stop(eof);
		".\n"			->  key_stop(eof);
		{error, Reason} ->  Stderr ! {self(), stderr, io_lib:format("error: ~p~n", [Reason])};
		Line			->  Stdout ! {self(), stdout, Line}
	end,
	key_loop(Stdin, Stdout, Stderr).

key_stop(Reason) ->
    io:format("Stopping: ~p ~p~n", [Reason, self()]),
    exit(Reason).