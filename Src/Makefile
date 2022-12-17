# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright (C) 2009-2022, Peter Johnson (gravatar.com/delphidabbler).
#
# Makefile for the CodeSnip project.


# Define macros relative paths to various directories relative to the repo root
BUILD_ROOT = _build
BIN_ROOT = $(BUILD_ROOT)\bin
EXE_ROOT = $(BUILD_ROOT)\exe
RELEASE_ROOT = $(BUILD_ROOT)\release
DOCS_ROOT = Docs
SRC_ROOT = Src

# Defines macros giving directories relative to location of the Makefile
BIN_REL = ..\$(BIN_ROOT)
EXE_REL = ..\$(EXE_ROOT)

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

# Resource files are compiled to the directory specified by BIN_REL macro.
.rc.res:
  @echo +++ Compiling Resource file $< +++
  @$(BRCC32) $< -fo$(BIN_REL)\$(@F)

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
  @if not exist $(BUILD_ROOT) mkdir $(BUILD_ROOT)
  @if exist $(BIN_ROOT) rmdir /S /Q $(BIN_ROOT)
  @mkdir $(BIN_ROOT)
  @if not exist $(EXE_ROOT) mkdir $(EXE_ROOT)
  @if not exist $(RELEASE_ROOT) mkdir $(RELEASE_ROOT)
  @cd $(SRC_ROOT)

# Builds CodeSnip pascal files and links program
pascal: CodeSnip.exe

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
CodeSnip.exe:
  @echo +++ Compiling Pascal +++
!ifdef PORTABLE
  @if exist $(EXE_REL)\$(@F) copy $(EXE_REL)\$(@F) $(EXE_REL)\$(@F).bak
!endif
  @$(DCC32) $(@B).dpr -B  $(DELPHIDEFINES)
!ifdef PORTABLE
  @copy $(EXE_REL)\$(@F) $(EXE_REL)\$(@B)-p.exe /Y
  @del $(EXE_REL)\$(@F)
  @if exist $(EXE_REL)\$(@F).bak copy $(EXE_REL)\$(@F).bak $(EXE_REL)\$(@F)
  @if exist $(EXE_REL)\$(@F).bak del $(EXE_REL)\$(@F).bak
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
  @$(HTMLRES) -mHTML.hrc -o$(BIN_REL)\HTML.res -r -q

# Compiles type library from IDL
typelib:
  @$(GENTLB) .\ExternalObj.ridl -D$(BIN_REL) -TExternalObj.tlb

# Builds setup program
setup:
!ifndef PORTABLE
  @del $(EXE_REL)\CodeSnip-Setup-*
  @$(ISCC) Install\CodeSnip.iss
!else
  @echo **** Portable build - no setup file created ****
!endif

# Creates auto generated files
autogen:
  @$(TLIBIMP) -P+ -Ps+ -D.\AutoGen -FtIntfExternalObj $(BIN_REL)\ExternalObj.tlb
  @if exist .\AutoGen\IntfExternalObj.dcr del .\AutoGen\IntfExternalObj.dcr

# Build release files (.zip)
# If RELEASEFILENAME is defined by caller then it is used as name of zip file
# otherwise default zip file name is used, which depends on whether PORTABLE
# is defined.
# If VERSION is defined by caller then it is appended to RELEASEFILENAME,
# separated by a dash.
!ifndef RELEASEFILENAME
!ifndef PORTABLE
RELEASEFILENAME = codesnip-exe
!else
RELEASEFILENAME = codesnip-portable-exe
!endif
!endif
!ifdef VERSION
RELEASEFILENAME = $(RELEASEFILENAME)-$(VERSION)
!endif
OUTFILE = $(RELEASE_ROOT)\$(RELEASEFILENAME).zip
release:
  @echo ---------------------
  @echo Creating Release File
  @echo ---------------------
  @cd ..
  -@if exist $(OUTFILE) del $(OUTFILE)
!ifndef PORTABLE
  @$(ZIP) -j -9 $(OUTFILE) $(EXE_ROOT)\CodeSnip-Setup-*.exe $(DOCS_ROOT)\ReadMe.txt
!else
  @$(ZIP) -j -9 $(OUTFILE) $(EXE_ROOT)\CodeSnip-p.exe
  @$(ZIP) -j -9 $(OUTFILE) $(EXE_ROOT)\CodeSnip.chm
  @$(ZIP) -j -9 $(OUTFILE) $(DOCS_ROOT)\ReadMe.txt
  @$(ZIP) -j -9 $(OUTFILE) $(DOCS_ROOT)\License.html
!endif
  @cd $(SRC_ROOT)

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
  @cd $(SRC_ROOT)
