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

#
# Run as pose session
#

ifeq ($(DEV),yes)
	POSE	=	bin/dose
	ERL	=	erl -noshell -i dev -deps dev $(POSE)
	DEPS	=	ERL_FLAGS="-deps dev"
else
	POSE	=	bin/pose
	ERL	=	erl -noshell -i deps $(POSE)
endif

FOLD =		bin/folderl

include include/Header.mk

#
# Custom rules
#

.PHONY:	all good install push todo

all:	push good

good:
	@$(POSE) superl
	@$(POSE) posure

install: 	online
	@if [ "$(ONLINE)" == yes ]; \
		then $(CROWBAR:_cmds_=delete-deps clean get-deps compile doc); \
		else $(CROWBAR:_cmds_=clean); fi

push:		online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; then $(PUSHLIB); fi
	@$(MAKE) -f include/Common.mk push

#
# Run non-overridden common rules.
#

todo:
	@$(MAKE) -f include/Common.mk todo
	
%::			;
	@echo No custom target found.
	@$(MAKE) -f include/Common.mk $@