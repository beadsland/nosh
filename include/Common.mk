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
# Copyright 2013 Beads D. Land-Trujillo.  All Rights Reserved
# -----------------------------------------------------------------------
# CDDL HEADER END

include include/Header.mk

#
# All good rules
#

all:		push good

good:		compile
	@if [ "$(DEV)" == yes ]; \
		then (erl $(ERL_PATH) -i deps -noshell $(SUPERL)); \
		else (echo Good only in development); fi 

#
# Temporary todo rules pending proper 2do_go4 implementation
#

todo:		docs TODO.edoc
	@git add -f $(TODO_FILES)
	@git commit $(TODO_FILES) -m "updated todo"

docs:		neat
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
# Rules for managing dependencies
#

current:	online
	@if [ "$(ONLINE)" == yes ]; \
		then $(CROWBAR:_cmds_=update-deps compile doc); \
		else $(CROWBAR:_cmds_=compile doc); fi

clean: 		online
	@if [ "$(ONLINE)" == yes ]; \
		then $(CROWBAR:_cmds_=delete-deps clean get-deps); \
		else $(CROWBAR:_cmds_=clean); fi

push:		online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
		then $(PUSHGIT); fi