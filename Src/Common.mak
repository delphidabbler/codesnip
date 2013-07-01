# ------------------------------------------------------------------------------
# Common.mak
#
# Common code for inclusion in all make files. Defines common macros and rules.
# Files that require Common.mak must include it using the !include directive.
#
# $Rev$
# $Date$
#
# ***** BEGIN LICENSE BLOCK *****
#
# Version: MPL 1.1
#
# The contents of this file are subject to the Mozilla Public License Version
# 1.1 (the "License"); you may not use this file except in compliance with the
# License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
# the specific language governing rights and limitations under the License.
#
# The Original Code is Common.mak
#
# The Initial Developer of the Original Code is Peter Johnson
# (http://www.delphidabbler.com/).
#
# Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
# Johnson. All Rights Reserved.
#
# Contributors:
#   NONE
#
# ***** END LICENSE BLOCK *****
# ------------------------------------------------------------------------------


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

# Implicit rules

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
.dpr.exe:
  @echo +++ Compiling Delphi Project $< +++
  @$(DCC32) $< -B -U"$(INDY10)"

# Resource files are compiled to the directory specified by BIN macro, which
# must have been set by the caller.
.rc.res:
  @echo +++ Compiling Resource file $< +++
  @$(BRCC32) $< -fo$(BIN)\$(@F)

# Version info files are compiled by VIEd. A temporary .rc file is left behind
.vi.rc:
  @echo +++ Compiling Version Info file $< +++
  @$(VIED) .\$<
