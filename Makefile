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
			if [ "$$?" -eq "0" ]; then (echo yes); else (echo no); fi`

HIDE_EDOC_WARN	=	grep -v "cannot handle URI.*edoc-info"
SUCCINCT	=	grep -v "Entering directory" | grep -v "Leaving directory"

#
# Build rules start
#

all:		push-nosh current nosh

run:		compile nosh

nosh:	
	tabs -1	>/dev/null # requires ncurses (noterm doesn't know tabs)
	@erl -noshell -pa deps/superl/ebin -s superl -pa ebin -s noterm

good:	current
	@erl -noshell -pa deps/superl/ebin -s superl -s init stop

doc:
	@rebar doc | $(HIDE_EDOC_WARN) | $(SUCCINCT)
	
compile:
	@rebar compile doc | $(HIDE_EDOC_WARN) | $(SUCCINCT)

current:	push-libs
	@rebar update-deps compile doc | $(HIDE_EDOC_WARN) | $(SUCCINCT)

clean: 		online
	@if [ "$(ONLINE)" == yes ]; \
			then (rm -rf deps; rebar clean get-deps | $(SUCCINCT)); \
			else (rebar clean | $(SUCCINCT)); fi
	
online:	
	@if [ "$(ONLINE)" == yes ]; \
			then (echo "Working online"); \
			else (echo "Working offline"); fi

#
# Development rules
#

push:		push-nosh push-libs push-superl

push-nosh:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (git push origin master); fi

push-libs:	push-bin


push-bin:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (cd ../nosh_bin; git push origin master); fi
			
push-superl:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (cd ../superl; git push origin master); fi
