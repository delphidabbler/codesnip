# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
#
# $Rev$
# $Date$
#
# Makefile for building the CodeSnip help file.

# Define macros giving relative paths to other directories from location of
# makefile
EXE = ..\..\Exe

# Define macros for required tools

MAKE = "$(MAKEDIR)\Make.exe" -$(MAKEFLAGS)

!ifdef HHCROOT
HHC = "$(HHCROOT)\HHC.exe"
!else
HHC = HHC.exe
!endif

# Explicit rules

# Default is to build everything
everything: config help

# Configure folders
config:
  @echo Configuring Help File
  @if not exist $(EXE) mkdir $(EXE)

# Builds help file
help:
  -1 @$(HHC) .\CodeSnip.chm
