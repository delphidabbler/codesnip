# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
#
# $Rev$
# $Date$
#
# Makefile for the CodeSnip Portable Launcher project.


# Define macros giving relative paths to other directories from location of
# makefile
BINROOT = ..\..\Bin
BIN = $(BINROOT)\Portable
EXE = ..\..\Exe

# Check for required environment variables

!ifdef DELPHIXE
DELPHIROOT = $(DELPHIXE)
!endif
!ifndef DELPHIROOT
!error DELPHIROOT or DELPHIXE environment variable required.
!endif

!ifndef INDY10
!error INDY10 environment variable required.
!endif

# Define macros for required tools

MAKE = "$(MAKEDIR)\Make.exe" -$(MAKEFLAGS)

DCC32 = "$(DELPHIROOT)\Bin\DCC32.exe"

BRCC32 = "$(DELPHIROOT)\Bin\BRCC32.exe"

!ifdef VIEDROOT
VIED = "$(VIEDROOT)\VIEd.exe" -makerc
!else
VIED = VIEd.exe -makerc
!endif

# Command line options
DELPHIDEFINES =

# Implicit rules

# Resource files are compiled to the directory specified by BIN macro, which
# must have been set by the caller.
.rc.res:
  @echo +++ Compiling Resource file $< +++
  @$(BRCC32) $< -fo$(BIN)\$(@F)

# Version info files are compiled by VIEd. A temporary .rc file is left behind
.vi.rc:
  @echo +++ Compiling Version Info file $< +++
  @$(VIED) .\$<

# Explicit rules

# Default is to build everything and release
everything: config resources pascal

# Configure source folders
config:
  @echo Configuring CodeSnip Portable Launcher
  # Create .cfg file from template
  @copy /Y CodeSnipPortable.cfg.tplt CodeSnipPortable.cfg
  # Create build folders
  @if not exist $(BINROOT) mkdir $(BINROOT)
  @if exist $(BIN) del /Q $(BIN)\*.*
  @if not exist $(BIN) mkdir $(BIN)
  @if not exist $(EXE) mkdir $(EXE)

# Builds CodeSnip pascal files and links program
pascal: CodeSnipPortable.exe

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
CodeSnipPortable.exe:
  @echo +++ Compiling Pascal Portable Launcher +++
  @$(DCC32) $(@B).dpr -B $(DELPHIDEFINES)

# Compiles the resources and deletes intermediate file created by VIED
VERINFOFILEBASE = LauncherVerInfo
resources: $(VERINFOFILEBASE).res LauncherResources.res
  -@del $(VERINFOFILEBASE).rc
