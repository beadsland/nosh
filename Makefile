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
	DEV			=	yes
endif

HIDE_EDOC_WARN	=	grep -v "cannot handle URI.*edoc-info"
SUCCINCT	=	grep -v "Entering directory" | grep -v "Leaving directory"

#
# Build rules start
#

all:		push-nosh current nosh

run:		compile nosh

nosh:	
	tabs -1	>/dev/null # requires ncurses (noterm doesn't know tabs)
	erl -pa ebin -noshell -s noterm
	
compile:
	rebar compile doc | $(HIDE_EDOC_WARN)

current:	push-libs
	rebar update-deps compile doc | $(HIDE_EDOC_WARN)

clean: 
	rm -rf deps
	rebar clean get-deps
	
	
#
# Development rules
#

push:		push-nosh push-libs

push-nosh:
	if [ "$(DEV)" = yes ]; then (git push origin master); fi

push-libs:	push-bin


push-bin:
	if [ "$(DEV)" = yes ]; then (cd ../nosh_bin; git push origin master); fi