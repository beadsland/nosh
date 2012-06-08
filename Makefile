# -------------------
# Makefile for nosh
# -------------------

# CDDL HEADER START
# -----------------------------------------------------------------------
# The contents of this file are subject to the Common Development and 
# Distribution License, Version 1.0 (the "License"); you may not use 
# this file except in compliance with the License.  You should have 
# received a copy of the Common Development and Distribution License 
# along with this software.  If not, it can be retrieved online at 
# http://www.opensource.org/licenses/CDDL-1.0
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License.
# 
# When distributing Covered Code, include this CDDL Header Notice in
# each file and include the License file at CDDL-LICENSE.  If applicable
# add the following below the CDDL Header, with the fields enclosed
# by brackets replaced by your own identifying information.
# "Portions Copyright [year] [name of copyright owner]"
# 
# Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
# -----------------------------------------------------------------------
# CDDL HEADER END

SHELL	= 	/bin/sh

ifeq ($(COMPUTERNAME),GOVMESH-BOOK)
	DEV		=	yes
else
	DEV		=	no
endif

ifeq ($(shell which ping),/cygdrive/c/Windows/system32/ping)
	PING	=	ping -n 1
else
	PING	=	ping -c1
endif

ONLINE	=	`$(PING) www.google.com 2>&1 >/dev/null; \
			if [ "$$?" -eq "0" ]; then (echo yes); \
			else (echo no); fi`
TTY	=	`tty`

SUCCINCT	=	grep -v "Entering directory" \
				| grep -v "Leaving directory"
CROWBAR	=	rebar _cmds_ | $(SUCCINCT) 2>&1 | $(FOLD)

TODO_MORE	=	`wc -l TODO.edoc | awk '{print $$1 - 7}'`

ifeq ($(DEV),yes)
	POSE	=	-pa dev/pose/ebin
	ERL	=	erl -noshell -i dev -deps dev $(POSE)
else
	POSE	=	-pa deps/pose/ebin
	ERL	=	erl -noshell -i deps $(POSE)
endif

POSURE	=	-s pose start posure
SUPERL	=	-s pose start superl
FOLDERL	=	-s pose start folderl
NOTERM	=	$(SUPERL) $(POSURE) -s pose start noterm
STOP 	= 	-s init stop

ORAGAMI	=	$(shell echo folderl | bin/pose folderl 2>&1 \
					| grep '^folderl$$')

ifeq ($(ORAGAMI),folderl)
	FOLD	= bin/pose folderl
else
	FOLD	= sed -nu ':p;s/\([^\n]\{80\}\)\([^\n]\)/\1\n \2/;tp;p'
endif

PUSHFOR	= for file in dev/*; do sh -c "cd $$file; git push origin master"; done

#
# Execution rules start
#

all:		current push-nosh nosh

run:		compile nosh

nosh:	tabs
	@if [ "$(TTY)" == "not a tty" ]; \
		then ($(ERL) $(NOTERM) echo $(STOP)); \
		else ($(ERL) $(NOTERM) $(STOP)); \
	fi 2>&1 | $(FOLD)

tabs:
	@if [ "$(TTY)" != "not a tty" ]; then (tabs -1 >/dev/null); fi

#
# Build rules start
#

good:	compile
	@$(ERL) $(SUPERL) $(POSURE) $(STOP)

doc:	compile
	
compile:	todo
	@rm -f *.dump doc/*.md doc/*.html
	@if [ "$(DEV)" == yes ]; \
		then REBAR_DEPS=dev; export REBAR_DEPS; fi; \
		$(CROWBAR:_cmds_=compile doc)

current:	push-libs todo
	@if [ "$(ONLINE)" == yes ]; then \
		$(CROWBAR:_cmds_=update-deps compile doc); else \
		$(CROWBAR:_cmds_=compile doc); fi

clean: 		online
	@if [ "$(ONLINE)" == yes ]; \
		then (rm -rf deps; rebar clean get-deps | $(SUCCINCT)); \
		else (rebar clean | $(SUCCINCT)); fi
	
online:	
	@if [ "$(ONLINE)" == yes ]; \
		then (echo "Working online"); \
		else (echo "Working offline"); fi

todo:
	@(head -7 TODO.edoc; \
	if [ $(TODO_MORE) -gt 0 ]; \
		then (echo "@todo ...plus $(TODO_MORE) more (see TODO.edoc)"); \
		fi) > doc/TODO_head.edoc

#
# Development rules start
#

push:		push-libs push-nosh

push-nosh:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
		then (git push origin master); fi

push-libs:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
		then (bash -c '$(PUSHFOR)'); fi
