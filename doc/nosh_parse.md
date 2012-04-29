

#Module nosh_parse#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This is a preliminary draft of the command line parser for `nosh`.



Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.3

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__* See [Shell Basics](http://sayle.net/book/basics.htm) for overview of functionality.  (to be implemented)
* See [Shell Command Language](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.md)
for detailed specification. (to be implemented)

<a name="types"></a>

##Data Types##




###<a name="type-block">block()</a>##



<pre>block() = nonempty_string() | {<a href="#type-context_type">context_type()</a>, [<a href="#type-block">block()</a>]}</pre>



###<a name="type-context_type">context_type()</a>##



<pre>context_type() = {eval, eval} | {quote, <a href="#type-group_type">group_type()</a>} | {quote, <a href="#type-quote_type">quote_type()</a>}</pre>



###<a name="type-group_type">group_type()</a>##



<pre>group_type() = line | pren | ifok | ambi | ifnz | pipe</pre>



###<a name="type-io_proc">io_proc()</a>##



<pre>io_proc() = pid()</pre>



###<a name="type-quote_type">quote_type()</a>##



<pre>quote_type() = back | doub | sing | escp | dbcp</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parse command line string and return a list of nested quoting and grouping context blocks,
or else <code>failed</code> on a caught syntax exception.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="parse-2"></a>

###parse/2##




<pre>parse(Subject::nonempty_string(), Stderr::<a href="#type-io_proc">io_proc()</a>) -> failed | [<a href="#type-block">block()</a>]</pre>
<br></br>






Parse command line string and return a list of nested quoting and grouping context blocks,
or else `failed` on a caught syntax exception.

Handle thrown errors for unmatched quoting and grouping characters.