%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Submodule of {@link nosh_parse}.
%%
%% <i>May be refactored back into nosh_parse at a later date.</i>
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: [...] Lists
%% TODO: [`'...`'] Lists command substituion
%% TODO: (...) second position parameter Lists
%% TODO: {...} Tuples
%% TODO: `<...>' Pids
%% TODO: `<<...>>' Bitstrings

%% @version 0.1.5
-module(nosh_context).
-version("0.1.5").

%%
%% Include files
%%

-define(debug, true).
-include("macro.hrl").

%%
%% Exported Functions
%%

-export([close_context/3, parse_context/3]).

%%
%% API Functions
%%

%% @doc Wind up context block.
-type context_type() :: nosh_parse:context_type().
-type context_desc() :: nosh_parse:context_desc().
-type context_stack() :: [context_desc()].
-type symbol() :: nonempty_string().
-type symbol_list() :: [symbol()].
-type context_list() :: nosh_parse:context_list().
-type context_result() :: {context_list(), context_stack()}.
-spec close_context(QType :: context_type(), Stack :: context_stack(),
                    List :: symbol_list()) -> context_result().
%
close_context(QType, Stack, List) ->
  ?DEBUG("close_context(~p, ~p, ~p)~n", [QType, Stack, List]),
  {Tail, _ReturnStack} = nosh_parse:parse({context, QType}, Stack, List),
  Close = {close_context, QType},
  Pred = fun(T) -> T /= Close end,
  {L1, L2} = lists:splitwith(Pred, Tail),
  if
    QType == dbcp, L1 == []	->
      Context = "\\";      % Didn't escape anything, so restore
                           % backslash as regular character.
    true                    ->
      Context = {{context, QType}, L1}
  end,
  {[Context | lists:delete(Close, L2)], Stack}.

%% @doc Unwind context and group stream.
-type close_result() :: {close_context, context_stack(), symbol_list()}.
-type parse_result() :: context_result() | close_result().
-spec parse_context(QType :: context_type(), Stack :: context_stack(),
                    List :: symbol_list()) -> parse_result().
%
parse_context(escp, Stack, List) -> parse_context_escp(Stack, List);
parse_context(dbcp, Stack, List) -> parse_context_dbcp(Stack, List);
parse_context(QT, Stack, [Symbol | Tail]) ->
  parse_context_symbol(QT, Stack, Symbol, Tail).

%%
%% Local Functions
%%

% Escape next character.
parse_context_escp([Type | Stack], [Head | Tail]) ->
  [First | Rest] = Head,
  Escape = [First, {close_context, escp}],
  {NewTail, Stack} = nosh_parse:parse(Type, Stack, [Rest | Tail]),
  {Escape ++ NewTail, [Type | Stack]}.

% Escape next character if escapable within double quotes.
parse_context_dbcp([Type | Stack], [Head | Tail]) ->
  [First | Rest] = Head,
  case [First] of
    "\$"    -> Escape = [[First], {close_context, dbcp}], Left = [Rest];
    "\`"    -> Escape = [[First], {close_context, dbcp}], Left = [Rest];
    "\""    -> Escape = [[First], {close_context, dbcp}], Left = [Rest];
    "\\"    -> Escape = [[First], {close_context, dbcp}], Left = [Rest];
    "\n"    -> Escape = throw({context, escp}), Left = [Rest];
    _Other  -> Escape = [{close_context, dbcp}], Left = [Head]
  end,
  {NewTail, Stack} = nosh_parse:parse(Type, Stack, [Left | Tail]),
  {Escape ++ NewTail, [Type | Stack]}.

% Parse symbols that switch to a nested context.
parse_context_symbol(QT, Stack, Symbol, Tail) ->
  case Symbol of
    "\n"    -> parse_symbol_eol(QT, Stack, Symbol, Tail);
    "\&"    -> parse_symbol_conjunct(QT, Stack, Symbol, Tail);
    "\|"    -> parse_symbol_conjunct(QT, Stack, Symbol, Tail);
    "\;"    -> parse_symbol_conjunct(QT, Stack, Symbol, Tail);
    "\("    -> parse_symbol_openparen(QT, Stack, Symbol, Tail);
    "\)"    -> parse_symbol_closeparen(QT, Stack, Symbol, Tail);
    "\`"    -> parse_symbol_backquote(QT, Stack, Symbol, Tail);
    "\""    -> parse_symbol_doubquote(QT, Stack, Symbol, Tail);
    "\'"    -> parse_symbol_singquote(QT, Stack, Symbol, Tail);
    "\\"    -> parse_symbol_backslash(QT, Stack, Symbol, Tail);
    _Other  -> parse_context_other(QT, Stack, Symbol, Tail)
  end.

% Parse end-of-line.
parse_symbol_eol(QT, Stack, Symbol, Tail) ->
  case QT   of
    line    -> {close_context, Stack, Tail};

    semi    -> {close_context, Stack, [Symbol | Tail]};
    ifok    -> {close_context, Stack, [Symbol | Tail]};
    ampi    -> {close_context, Stack, [Symbol | Tail]};
    ifnz    -> {close_context, Stack, [Symbol | Tail]};
    pipe    -> {close_context, Stack, [Symbol | Tail]};

    pren    -> throw({context, pren});
    back    -> throw({context, back});
    doub    -> throw({context, doub});
    sing    -> throw({context, sing});

    _Other  -> pass_context(QT, Stack, Symbol, Tail)
  end.

% Parse semicolons and single and double ampersands and vertical bars.
parse_symbol_conjunct(QT, Stack, Symbol, Tail)
     when QT == doub; QT == sing -> pass_context(QT, Stack, Symbol, Tail);
parse_symbol_conjunct(QT, Stack, Symbol, [Symbol | Tail])
     when Symbol == "\&"; Symbol == "\|" ->
  case Symbol of
    "\&" -> close_context(ifok, [{context, QT} | Stack], Tail);
    "\|" -> close_context(ifnz, [{content, QT} | Stack], Tail)
  end;
parse_symbol_conjunct(QT, Stack, Symbol, Tail) ->
  case Symbol of
    "\;" -> close_context(semi, [{context, QT} | Stack], Tail);
    "\&" -> close_context(ampi, [{context, QT} | Stack], Tail);
    "\|" -> close_context(pipe, [{content, QT} | Stack], Tail)
  end.

% Parse open parentheses entry to context.
parse_symbol_openparen(QT, Stack, Symbol, Tail) ->
  case QT of
    doub    -> pass_context(QT, Stack, Symbol, Tail);
    sing    -> pass_context(QT, Stack, Symbol, Tail);

    _Other  -> close_context(pren, [{context, QT} | Stack], Tail)
  end.

% Parse close parentheses exit from context.
parse_symbol_closeparen(QT, Stack, Symbol, Tail) ->
  case QT of
    doub    -> pass_context(QT, Stack, Symbol, Tail);
    sing    -> pass_context(QT, Stack, Symbol, Tail);

    pren    -> {close_context, Stack, Tail};
    line    -> throw({close, pren});

    _Other  -> {close_context, Stack, [Symbol | Tail]}
  end.

% Parse command substitution back ticks.
parse_symbol_backquote(QT, Stack, Symbol, Tail) ->
  case QT of
    sing    -> pass_context(QT, Stack, Symbol, Tail);

    back    -> {close_context, Stack, Tail};

    _Other  -> close_context(back, [{context, QT} | Stack], Tail)
  end.

% Parse substitution-friendly double quotes.
parse_symbol_doubquote(QT, Stack, Symbol, Tail) ->
  case QT of
    doub    -> {close_context, Stack, Tail};
    sing    -> pass_context(QT, Stack, Symbol, Tail);

    _Other  -> close_context(doub, [{context, QT} | Stack], Tail)
  end.

% Parse substitution-free single quotes.
parse_symbol_singquote(QT, Stack, Symbol, Tail) ->
  case QT of
    doub    -> pass_context(QT, Stack, Symbol, Tail);
    sing    -> {close_context, Stack, Tail};

    _Other  -> close_context(sing, [{context, QT} | Stack], Tail)
  end.

% Parse escaped character.
parse_symbol_backslash(QT, Stack, Symbol, Tail) ->
  case QT of
    doub    -> close_context(dbcp, [{context, QT} | Stack], Tail);
    sing    -> pass_context(QT, Stack, Symbol, Tail);

    _Other  -> close_context(escp, [{context, QT} | Stack], Tail)
  end.

% Parse any other symbols and words.
parse_context_other(QType, Stack, Head, Tail) ->
  {NewTail, _ReturnStack} = nosh_parse:parse({context, QType}, Stack, Tail),
  {[Head | NewTail], Stack}.

%% Pass non-symbols along unchanged (this should be tokenizing).
pass_context(QT, Stack, Symbol, Tail) ->
  {NewTail, _ReturnStack} = nosh_parse:parse({context, QT}, Stack, Tail),
  {[Symbol | NewTail], Stack}.