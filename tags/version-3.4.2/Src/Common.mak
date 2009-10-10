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
# Portions created by the Initial Developer are Copyright (C) 2009 Peter
# Johnson. All Rights Reserved.
#
# Contributors:
#   NONE
#
# ***** END LICENSE BLOCK *****
# ------------------------------------------------------------------------------


# Requires that the BIN macro is defined to point to directory that is to
# receive .res and .dcu output.

# The preferred compiler is Delphi 2006. If the DELPHI2006 evironment variable
# is set, it will be used and expected to Delphi 2006 install directory.
# If DELPHI2006 is not set then the DELPHIROOT environment variable is examined.
# This can be set to any Delphi compiler (should compile if later than Delphi
# 2006). If neither DELPHI2006 nor DELPHIROOT is set then a Delphi compiler is
# expected to be present on the system path.
!ifdef DELPHI2006
DELPHIROOT = $(DELPHI2006)
!endif

# Define path to DelphiDabbler library code if specified for Delphi 2006
!ifdef DELPHIDABLIBD2006
DELPHIDABLIB = $(DELPHIDABLIBD2006);
!endif

# Check for required macros
!ifndef DELPHIROOT
!error DELPHIROOT environment variable required.
!endif
!ifndef DELPHIDABLIB
!error DELPHIDABLIB environment variable required.
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
  @$(DCC32) $< -B -U"$(DELPHIDABLIB)" -U"$(INDY10)"

# Resource files are compiled to the directory specified by BIN macro, which
# must have been set by the caller.
.rc.res:
  @echo +++ Compiling Resource file $< +++
  @$(BRCC32) $< -fo$(BIN)\$(@F)

# Version info files are compiled by VIEd. A temporary .rc file is left behind
.vi.rc:
  @echo +++ Compiling Version Info file $< +++
  @$(VIED) .\$<
