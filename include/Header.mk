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

#
# Figure out if running GNU make
#

MAKE_HEAD	= make -v | head -1
MAKE_WORD	= $(MAKE_HEAD) | sed 's/\ .*//'
MAKE_VEND	= $(shell $(MAKE_WORD))

ifneq ($(IS_SUBMAKE),true)
   $(info $(shell $(MAKE_HEAD)))
endif

ifneq ($(MAKE_VEND),GNU)
   $(warning Not running GNU make -- something might break)
endif

#
# Figure out if development or production
#

ifeq ($(COMPUTERNAME),GOVMESH-BOOK)
	DEV		=	$(if $(PROD),no,yes)
else
	DEV		=	no
endif

ifeq ($(DEV)$(IS_SUBMAKE),yes)
   $(info Development environment)
endif

#
# Figure out if online or offline
#

ifeq ($(shell which ping),/cygdrive/c/Windows/system32/ping)
	PING	=	ping -n 1
else
	PING	=	ping -c1
endif

ONTEST	= $(PING) www.google.com 2>&1 >/dev/null \
			&& echo online || echo offline
ONBOOL	= (test "$(ONRESULT)" == "online" \
			&& echo yes || echo no)

ifneq ($(IS_SUBMAKE),true)	   # if submake, online in subpass
   ONRESULT = $(shell $(ONTEST))
   $(info Working $(ONRESULT))
   ONLINE = $(shell $(ONBOOL))
endif

#
# Macros for commands
#

REBAR = 	`command -v rebar || echo bin/rebar`
GREP =		grep --line-buffered
SUCCINCT =	$(GREP) -v "Entering directory" | $(GREP) -v "Leaving directory"
FOLD = 		cat
CROWBAR	=	$(SUBPASS) $(REBAR) _cmds_ | $(SUCCINCT) 2>&1 | $(FOLD)

B_PREFIX = 	.unison/
B_SUFFIX =	.0.bak
MERGE =		Name *.mk -> diff3 -m CURRENT1 CURRENTARCH CURRENT2 > NEW
UNISON =	unison ./include ../nosh/include -batch -terse \
				-ignore "Name *.hrl" -ignore "Name .unison/*" \
				-backupcurrent "Name *" -backuplocation local \
				-backupprefix '$(B_PREFIX)' -backupsuffix '.$$VERSION.bak'
				
SUBMAKE		= $(MAKE) --no-print-directory _param_ \
				IS_SUBMAKE=true PROD=$(PROD) $(SUBPASS)
COMMAKE		= $(SUBMAKE:_param_=-f include/Common.mk $@)

#
# Macros for good
#

ifndef DEPS
	DEPS = 		deps
endif
SUBPASS =	DEPS="$(DEPS)" ONLINE="$(ONLINE)"

ifndef POSEBIN
	POSEBIN = 	$(DEPS)/pose/ebin
endif

ERL	=		erl -noshell -i $(DEPS) -deps $(DEPS) -pa $(POSEBIN)

POSURE	=	-s pose start posure
ifndef SUPERL
	SUPERL	=	-s pose start superl
endif
NOTERM	=	-s pose start noterm
STOP	=	-s init stop

#
# Todo logic pending proper 2do_go4 implementation
#

ifeq ($(wildcard TODO.edoc),TODO.edoc)
	TODO_MORE =	`wc -l TODO.edoc | awk '{print $$1 - 7}'`
else
	TODO_MORE =	0
endif
TODO_FILES =	$(wildcard TODO.edoc) \
					README.md doc/README.md doc/TODO_head.edoc