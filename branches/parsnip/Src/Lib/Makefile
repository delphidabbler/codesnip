# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
#
# $Rev$
# $Date$
#
# Makefile for libraries required to build CodeSnip.


# Define macros giving relative paths to other directories from location of
# makefile
BIN = ..\..\Bin
LIBBIN = $(BIN)\Lib
PKGBIN = $(LIBBIN)\Pkg

# Check for required environment variables

!ifdef DELPHIXE
DELPHIROOT = $(DELPHIXE)
!endif
!ifndef DELPHIROOT
!error DELPHIROOT or DELPHIXE environment variable required.
!endif

# Define macros for required tools

MAKE = "$(MAKEDIR)\Make.exe" -$(MAKEFLAGS)

DCC32 = "$(DELPHIROOT)\Bin\DCC32.exe"

# Command line options
DELPHIDEFINES =

# Explicit rules

everything: config libs

# Configure source folders
config:
  @echo Configuring Libraries
  @if not exist "$(BIN)" mkdir "$(BIN)"
  @if not exist "$(LIBBIN)" mkdir "$(LIBBIN)"
  @if not exist "$(PKGBIN)" mkdir "$(PKGBIN)"
  @del $(LIBBIN)\*.* /Q /S


# Make libraries
libs: Collections.bpl SynEdit_RXE.bpl DDabLib.bpl

Collections.bpl:
  @cd DelphiColl
  @$(DCC32) $(@B).dpk -B $(DELPHIDEFINES)
  @cd ..

SynEdit_RXE.bpl:
  @cd SynEdit
  @$(DCC32) $(@B).dpk -B $(DELPHIDEFINES)
  @cd ..

DDabLib.bpl:
  @cd DDabLib
  @$(DCC32) $(@B).dpk -B $(DELPHIDEFINES)
  @cd ..
