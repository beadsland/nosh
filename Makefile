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

#
# Figure out if development or production
#

ifeq ($(COMPUTERNAME),GOVMESH-BOOK)
	DEV		=	$(if $(PROD),no,yes)
else
	DEV		=	no
endif
	
ifeq ($(DEV),yes)
	POSE	=	bin/dose
	ERL	=	erl -noshell -i dev -deps dev $(POSE)
	DEPS	=	ERL_FLAGS="-deps dev"
else
	POSE	=	bin/pose
	ERL	=	erl -noshell -i deps $(POSE)
endif

#
# Figure out if online or offline
#

ifeq ($(shell which ping),/cygdrive/c/Windows/system32/ping)
	PING	=	ping -n 1
else
	PING	=	ping -c1
endif

ONLINE	= `$(PING) www.google.com 2>&1 >/dev/null; \
		if [ "$$?" -eq "0" ]; then (echo yes); \
		else (echo no); fi`

#
# Macros for commands invoked by rules
#

TTY	=	`tty`

REBAR = 	`command -v rebar || echo bin/rebar`
SUCCINCT =	grep -v "Entering directory" | grep -v "Leaving directory"
CROWBAR	=	$(DEPS) $(REBAR) _cmds_ | $(SUCCINCT) 2>&1 | $(FOLD)
FOLD =		bin/folderl

TODO_MORE =	`wc -l TODO.edoc | awk '{print $$1 - 7}'`
TODO_FILES =	TODO.edoc README.md doc/README.md doc/TODO_head.edoc

PUSHGIT = 	git push origin master
PUSHDO	=	echo "cd $$file; $(PUSHGIT)" | /bin/sh
PUSHFOR =	for file in dev/*; do $(PUSHDO); done
#
# Build rules
#

good:
	@$(POSE) superl
	@$(POSE) posure

todo:		doc
	@git add -f $(TODO_FILES)
	@git commit $(TODO_FILES) -m "updated todo"

doc:		neat
	@$(CROWBAR:_cmds_=doc)
	@(head -7 TODO.edoc; \
		if [ $(TODO_MORE) -gt 0 ]; \
		then (echo "@todo ...plus $(TODO_MORE) more (see TODO.edoc)"); \
		fi) > doc/TODO_head.edoc

compile:	neat
	@$(CROWBAR:_cmds_=compile doc)

neat:
	@rm -f *.dump doc/*.md doc/*.html

#
# Deps rules
#

current:
	@if [ "$(ONLINE)" == yes ]; then \
		$(CROWBAR:_cmds_=update-deps compile doc); else \
		$(CROWBAR:_cmds_=compile doc); fi

clean: 		online
	@if [ "$(ONLINE)" == yes ]; \
		then (rm -rf deps; rebar clean get-deps | $(SUCCINCT)); \
		else (rebar clean | $(SUCCINCT)); fi
	
install: 	online
	@if [ "$(ONLINE)" == yes ]; \
		then (rm -rf deps; rebar clean get-deps compile doc \
						| $(SUCCINCT)); \
		else (rebar clean | $(SUCCINCT)); fi
	
online:	
	@if [ "$(ONLINE)" == yes ]; \
		then (echo "Working online"); \
		else (echo "Working offline"); fi

#
# Push rules
#

push:		push-libs push-nosh

push-nosh:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
		then $(PUSHGIT); fi

push-libs:	mkdev online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
		then (echo '$(PUSHFOR)' | /bin/bash); fi

mkdev: bin/mkdev
	bin/mkdev