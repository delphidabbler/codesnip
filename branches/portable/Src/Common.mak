# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
#
# $Rev$
# $Date$
#
# Common code for inclusion in all make files. Defines common macros and rules.
# Files that require Common.mak must include it using the !include directive.


# Required macros
# ---------------
#
# Make will fail if any of the following macros is not defined.
#
# BIN
#   Must be defined in the calling script to address the directory that is to
#   receive .res and .dcu output files
#
# DELPHIROOT (or DELPHI2010)
#   DELPHIROOT must address the installation directory of the required Delphi
#   compiler (Delphi 2010 as a minimum). If Delphi 2010 is to be used the
#   DELPHI2010 macro can be defined to address the Delphi 2010 installation
#   directory. When DELPHI2010 is defined any existing value of DELPHIROOT is
#   ignored and DELPHI2010 is used instead.
#
# INDY10
#   Must be set to the directory where the Indy 10 components are installed. The
#   components must have been built with the same version of Delphi that is to
#   be used to compile the program.


# Check for DELPHI2010 macro and use it for DELPHIROOT if set.

!ifdef DELPHI2010
DELPHIROOT = $(DELPHI2010)
!endif

# Check for required macros

!ifndef DELPHIROOT
!error DELPHIROOT environment variable required.
!endif
!ifndef INDY10
!error INDY10 environment variable required.
!endif
!ifndef BIN
!error BIN macro must be defined in calling script.
!endif

# Define common macros that access required build tools

MAKE = "$(MAKEDIR)\Make.exe" -$(MAKEFLAGS)
DCC32 = "$(DELPHIROOT)\Bin\DCC32.exe"
BRCC32 = "$(DELPHIROOT)\Bin\BRCC32.exe"
!ifdef VIEDROOT
VIED = "$(VIEDROOT)\VIEd.exe" -makerc
!else
VIED = VIEd.exe -makerc
!endif

# Command line options
!ifdef PORTABLE
DELPHIDEFINES = "-DPORTABLE"
!else
DELPHIDEFINES =
!endif

# Implicit rules

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
.dpr.exe:
  @echo +++ Compiling Delphi Project $< +++
  @$(DCC32) $< -B -U"$(INDY10)" $(DELPHIDEFINES)

# Resource files are compiled to the directory specified by BIN macro, which
# must have been set by the caller.
.rc.res:
  @echo +++ Compiling Resource file $< +++
  @$(BRCC32) $< -fo$(BIN)\$(@F)

# Version info files are compiled by VIEd. A temporary .rc file is left behind
.vi.rc:
  @echo +++ Compiling Version Info file $< +++
  @$(VIED) .\$<
