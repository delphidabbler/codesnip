# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
#
# $Rev$
# $Date$
#
# Makefile for the CodeSnip installer.


# Define macros giving relative paths to other directories from location of
# makefile
EXE = ..\..\Exe

# Define macros for required tools

!ifdef INNOSETUP_U
INNOSETUP = $(INNOSETUP_U)
!endif
!ifdef INNOSETUP
ISCC = "$(INNOSETUP)\ISCC.exe"
!else
ISCC = ISCC.exe
!endif

# Explicit rules

# Default is configure and build setup
everything: config setup

# Configure folders
config:
  @echo Configuring Setup
  @if not exist $(EXE) mkdir $(EXE)

# Builds setup program
setup:
  @del $(EXE)\CodeSnip-Setup-*
  @$(ISCC) CodeSnip.iss
