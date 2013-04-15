#/usr/bin/env bash

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

POSEBIN=nosh/bin
SCRDIR=$(dirname ${BASH_SOURCE[0]})
APPDIR=$SCRDIR/..

function findpath() {
	while [ ${#} -gt 0 ]; do
		PATH=$APPDIR/${1}/$POSEBIN/pose
		if [ -f $PATH ]; then echo $PATH; break; fi
		shift
	done
	}

POSE=$(findpath dev deps ..)

if [ ! "$POSE" ]; 
	then (echo Failed to locate pose wrapper; exit 1); fi

POSEPATH=$(dirname $POSE)
cd $POSEPATH
./pose $@