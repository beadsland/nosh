

#Module nosh_beam#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Beam binary utility functions used by [`nosh_load`](nosh_load.md).



Copyright (c) 2012 Beads D. Land-Trujillo

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-attribute">attribute()</a>##



<pre>attribute() = atom()</pre>



###<a name="type-beam_lib_error">beam_lib_error()</a>##



<pre>beam_lib_error() = {beam_lib, term()}</pre>



###<a name="type-binary_detail_error">binary_detail_error()</a>##



<pre>binary_detail_error() = <a href="#type-beam_lib_error">beam_lib_error()</a> | {missing_chunk, <a href="#type-attribute">attribute()</a>}</pre>



###<a name="type-file_error_reason">file_error_reason()</a>##



<pre>file_error_reason() = <a href="#type-posix">posix()</a> | badarg | terminated | system_limit</pre>



###<a name="type-filename">filename()</a>##



<pre>filename() = <a href="file.md#type-filename">file:filename()</a></pre>



###<a name="type-package">package()</a>##



<pre>package() = term()</pre>



###<a name="type-posix">posix()</a>##



<pre>posix() = atom()</pre>



###<a name="type-slurp_error">slurp_error()</a>##



<pre>slurp_error() = {read, <a href="#type-file_error_reason">file_error_reason()</a>} | <a href="#type-beam_lib_error">beam_lib_error()</a> | no_module</pre>



###<a name="type-version">version()</a>##



<pre>version() = term()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_binary_detail-2">get_binary_detail/2</a></td><td>Get version and package of binary.</td></tr><tr><td valign="top"><a href="#slurp_binary-1">slurp_binary/1</a></td><td>Read binary file into memory.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_binary_detail-2"></a>

###get_binary_detail/2##




<pre>get_binary_detail(Module::module(), Binary::binary()) -> {ok, <a href="#type-version">version()</a>, <a href="#type-package">package()</a>} | {error, <a href="#type-binary_detail_error">binary_detail_error()</a>}</pre>
<br></br>




Get version and package of binary<a name="slurp_binary-1"></a>

###slurp_binary/1##




<pre>slurp_binary(Filename::<a href="#type-filename">filename()</a>) -> {ok, module(), binary()} | {error, <a href="#type-slurp_error">slurp_error()</a>}</pre>
<br></br>




Read binary file into memory.