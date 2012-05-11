

#Module nosh_util#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Utility function library.



Copyright (c) 2012 Beads D. Land-Trujillo

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">spec API functions</font>

<a name="types"></a>

##Data Types##




###<a name="type-date_time">date_time()</a>##



<pre>date_time() = <a href="calendar.md#type-date_time">calendar:date_time()</a></pre>



###<a name="type-file_error">file_error()</a>##



<pre>file_error() = {error, {atom(), <a href="#type-filename">filename()</a>}}</pre>



###<a name="type-filename">filename()</a>##



<pre>filename() = string()</pre>



###<a name="type-folder">folder()</a>##



<pre>folder() = nonempty_string()</pre>



###<a name="type-parallel_result">parallel_result()</a>##



<pre>parallel_result() = {false, <a href="#type-path_string">path_string()</a>} | {true, <a href="#type-path_string">path_string()</a>} | {true, <a href="#type-path_string">path_string()</a>, <a href="#type-project">project()</a>}</pre>



###<a name="type-path">path()</a>##



<pre>path() = <a href="#type-path_string">path_string()</a> | <a href="#type-path_list">path_list()</a></pre>



###<a name="type-path_list">path_list()</a>##



<pre>path_list() = {folders, [<a href="#type-folder">folder()</a>]}</pre>



###<a name="type-path_string">path_string()</a>##



<pre>path_string() = nonempty_string()</pre>



###<a name="type-project">project()</a>##



<pre>project() = atom()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#can_read-1">can_read/1</a></td><td>Test if file or directory is readable.</td></tr><tr><td valign="top"><a href="#can_write-1">can_write/1</a></td><td>Test if file or directory is writeable.</td></tr><tr><td valign="top"><a href="#find_parallel_folder-3">find_parallel_folder/3</a></td><td>Walk absolute directory path, finding where parallel would occur.</td></tr><tr><td valign="top"><a href="#last_modified-1">last_modified/1</a></td><td>Get last date and time file last modified.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="can_read-1"></a>

###can_read/1##




<pre>can_read(Filename::<a href="#type-filename">filename()</a>) -> boolean() | <a href="#type-file_error">file_error()</a></pre>
<br></br>




Test if file or directory is readable.<a name="can_write-1"></a>

###can_write/1##




<pre>can_write(Filename::<a href="#type-filename">filename()</a>) -> boolean() | <a href="#type-file_error">file_error()</a></pre>
<br></br>




Test if file or directory is writeable.<a name="find_parallel_folder-3"></a>

###find_parallel_folder/3##




<pre>find_parallel_folder(OldFlder::<a href="#type-folder">folder()</a>, NewFolder::<a href="#type-folder">folder()</a>, OldPath::<a href="#type-path">path()</a>) -> <a href="#type-parallel_result">parallel_result()</a></pre>
<br></br>




Walk absolute directory path, finding where parallel would occur.<a name="last_modified-1"></a>

###last_modified/1##




<pre>last_modified(Filename::<a href="#type-filename">filename()</a>) -> {ok, <a href="#type-date_time">date_time()</a>} | <a href="#type-file_error">file_error()</a></pre>
<br></br>




Get last date and time file last modified.