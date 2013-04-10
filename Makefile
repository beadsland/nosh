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
# Copyright 2013 Beads D. Land-Trujillo.  All Rights Reserved.
# -----------------------------------------------------------------------
# CDDL HEADER END

# -------------------
# Makefile for nosh
# -------------------

include include/Header.mk

ifeq ($(DEV),yes)
	DEPS	=	dev
	SUBPASS =	DEPS="$(DEPS)"
endif

FOLD =		bin/folderl

PUSHMAKE	= if [ -e $@/include/Common.mk ]; \
				then $(SUBMAKE:_param_=-C $@ push); \
				else (cd $@; git push origin master); fi

#
# Custom rules
#

.PHONY:	all install push force todo

all:	push compile good

install:
	@if [ "$(ONLINE)" == yes ]; \
		then $(CROWBAR:_cmds_=delete-deps clean get-deps compile doc); \
		else $(CROWBAR:_cmds_=clean); fi

clean:
	@if [ "$(DEV)" == yes ]; \
		then (rm dev/*; rmdir dev; bin/mkdev); fi
	@$(COMMAKE)

push:	$(wildcard dev/*/.git)
	@$(COMMAKE)

dev/%/.git:		force
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
		then (echo -n "$*: "; $(PUSHMAKE)); fi	

make:	$(wildcard dev/*/include/Common.mk)
	@echo Unison of make includes

dev/%/include/Common.mk:	force
	@echo $@
	@cd dev/$*; $(SUBMAKE:_param_=-f include/Common.mk make)

force:	;

#
# Run non-overridden common rules.
#

todo:
	@$(COMMAKE)
	
%::			;
	@echo No custom target found
	@$(COMMAKE)