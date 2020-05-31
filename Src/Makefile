# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
#
# Makefile for the CodeSnip project.


# Define macros giving relative paths to other directories from location of
# makefile
BIN = ..\Bin
EXE = ..\Exe

# Check for required environment variables

!ifdef DELPHIXE
DELPHIROOT = $(DELPHIXE)
!endif
!ifndef DELPHIROOT
!error DELPHIROOT or DELPHIXE environment variable required.
!endif

# Define macros for required tools

TLIBIMP = "$(DELPHIROOT)\Bin\TLibImp.exe"

GENTLB = "$(DELPHIROOT)\Bin\GenTLB.exe"

MAKE = "$(MAKEDIR)\Make.exe" -$(MAKEFLAGS)

DCC32 = "$(DELPHIROOT)\Bin\DCC32.exe"

BRCC32 = "$(DELPHIROOT)\Bin\BRCC32.exe"

!ifdef VIEDROOT
VIED = "$(VIEDROOT)\VIEd.exe" -makerc
!else
VIED = VIEd.exe -makerc
!endif

!ifdef HTMLRESROOT
HTMLRES = "$(HTMLRESROOT)\HTMLRes.exe"
!else
HTMLRES = HTMLRes
!endif

!ifdef INNOSETUP_U
INNOSETUP = $(INNOSETUP_U)
!endif
!ifdef INNOSETUP
ISCC = "$(INNOSETUP)\ISCC.exe"
!else
ISCC = ISCC.exe
!endif

!ifdef HHCROOT
HHC = "$(HHCROOT)\HHC.exe"
!else
HHC = HHC.exe
!endif

!ifdef ZIPROOT
ZIP = "$(ZIPROOT)\Zip.exe"
!else
ZIP = Zip.exe
!endif

# Command line options
!ifdef PORTABLE
DELPHIDEFINES = "-DPORTABLE"
!else
DELPHIDEFINES =
!endif

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
everything: config exes setup release

# Builds CodeSnip exe file + help
exes: codesnip help

# Build CodeSnip executable
codesnip: typelib resources autogen pascal

# Configure source folders
config:
  @echo Configuring CodeSnip
  # Create .cfg file from template
  @copy /Y CodeSnip.cfg.tplt CodeSnip.cfg
  # Create build folders
  @cd ..
  @if exist Bin rmdir /S /Q Bin
  @mkdir Bin
  @if not exist Exe mkdir Exe
  @if not exist Release mkdir Release
  @cd Src

# Builds CodeSnip pascal files and links program
pascal: CodeSnip.exe

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
CodeSnip.exe:
  @echo +++ Compiling Pascal +++
!ifdef PORTABLE
  @if exist $(EXE)\$(@F) copy $(EXE)\$(@F) $(EXE)\$(@F).bak
!endif
  @$(DCC32) $(@B).dpr -B  $(DELPHIDEFINES)
!ifdef PORTABLE
  @copy $(EXE)\$(@F) $(EXE)\$(@B)-p.exe /Y
  @del $(EXE)\$(@F)
  @if exist $(EXE)\$(@F).bak copy $(EXE)\$(@F).bak $(EXE)\$(@F)
  @if exist $(EXE)\$(@F).bak del $(EXE)\$(@F).bak
!endif

# Builds help file
help:
  -1 @$(HHC) .\Help\CodeSnip.chm

# Compiles the resources and deletes intermediate file created by VIED
!ifndef PORTABLE
VERINFOFILEBASE = VCodeSnip
!else
VERINFOFILEBASE = VCodeSnipPortable
!endif
resources: $(VERINFOFILEBASE).res Resources.res HTML.res
  -@del $(VERINFOFILEBASE).rc

# Compiles HTMLres from .hrc file
HTML.res: HTML.hrc
  @echo +++ Compiling HTML Resource manifest file +++
  @$(HTMLRES) -mHTML.hrc -o$(BIN)\HTML.res -r -q

# Compiles type library from IDL
typelib:
  @$(GENTLB) .\ExternalObj.ridl -D$(BIN) -TExternalObj.tlb

# Builds setup program
setup:
!ifndef PORTABLE
  @del ..\Exe\CodeSnip-Setup-*
  @$(ISCC) Install\CodeSnip.iss
!else
  @echo **** Portable build - no setup file created ****
!endif

# Creates auto generated files
autogen:
  @$(TLIBIMP) -P+ -Ps+ -D.\AutoGen -FtIntfExternalObj $(BIN)\ExternalObj.tlb
  @if exist .\AutoGen\IntfExternalObj.dcr del .\AutoGen\IntfExternalObj.dcr

# Build release files (.zip)
!ifndef RELEASEFILENAME
RELEASEFILENAME = dd-codesnip
!ifdef PORTABLE
RELEASEFILENAME = $(RELEASEFILENAME)-portable
!endif
!endif
OUTFILE = Release\$(RELEASEFILENAME).zip
release:
  @echo ---------------------
  @echo Creating Release File
  @echo ---------------------
  @cd ..
  -@if exist $(OUTFILE) del $(OUTFILE)
!ifndef PORTABLE
  @$(ZIP) -j -9 $(OUTFILE) Exe\CodeSnip-Setup-*.exe Docs\ReadMe.txt
!else
  @$(ZIP) -j -9 $(OUTFILE) Exe\CodeSnip-p.exe
  @$(ZIP) -j -9 $(OUTFILE) Exe\CodeSnip.chm
  @$(ZIP) -j -9 $(OUTFILE) Docs\ReadMe.txt
  @$(ZIP) -j -9 $(OUTFILE) Docs\License.html
!endif
  @cd Src

# Clean up unwanted files
clean:
  @cd ..
  # remove unwanted files
  -@del /S *.~* 2>nul
  -@del /S ~* 2>nul
  -@del /S *.dsk 2>nul
  -@del /S *.local 2>nul
  -@del /S *.identcache 2>nul
  -@del /S *.tvsconfig 2>nul
  # remove __history folders
  -@for /F "usebackq" %i in (`dir /S /B /A:D ..\__history`) do @rmdir /S /Q %i
  @cd Src
