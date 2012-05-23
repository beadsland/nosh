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


HIDE_EDOC_WARN	=	grep -v "cannot handle URI.*edoc-info"
SUCCINCT	=	grep -v "Entering directory" \
				| grep -v "Leaving directory"
HIDE_TEST_WARN	=	grep -v "edoc: warning: file.*test.erl' belongs"
CROWBAR		=	rebar _cmds_ | $(HIDE_EDOC_WARN) | $(SUCCINCT) \
				| $(HIDE_TEST_WARN)

POSE	=	-pa deps/pose/ebin
ERL	=	erl -noshell -i deps $(POSE)
POSURE	=	-s pose start posure
SUPERL	=	-s pose start superl
NOTERM	=	-s pose start noterm
ERLSTOP = 	-s init stop

TODO_MORE	=	`wc -l TODO.edoc | awk '{print $$1 - 7}'`

#
# Execution rules start
#

all:		current push-nosh nosh

run:		compile nosh

nosh:	tabs
	@if [ "$(TTY)" == "not a tty" ]; \
		then ($(ERL) $(SUPERL) $(POSURE) $(NOTERM) echo $(ERLSTOP)); \
		else ($(ERL) $(SUPERL) $(POSURE) $(NOTERM) $(ERLSTOP)); fi

tabs:
	@if [ "$(TTY)" != "not a tty" ]; then (tabs -1 >/dev/null); fi

#
# Build rules start
#

good:	compile
	@$(ERL) $(SUPERL) $(POSURE) $(ERLSTOP)

doc:	compile
	
compile:	todo
	@rm -f doc/edoc-info *.dump
	@$(CROWBAR:_cmds_=compile doc)

current:	push-libs todo
	@if [ "$(ONLINE)" == yes ]; then \
		$(CROWBAR:_cmds_=update-deps compile doc); else \
		$(CROWBAR:_cmds_=compile doc); fi

clean: 		online
	@rm -f doc/*.md doc/*.html
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

push:		push-libs push-superl push-nosh

push-nosh:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (git push origin master); fi

push-libs:	push-noterm push-pose push-bin push-erl push-superl

push-noterm:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (cd ../noterm; git push origin master); fi

push-pose:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (cd ../pose; git push origin master); fi

push-bin:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (cd ../nosh_bin; git push origin master); fi

push-erl:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (cd ../nosh_erl; git push origin master); fi

push-superl:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (cd ../superl; git push origin master); fi
