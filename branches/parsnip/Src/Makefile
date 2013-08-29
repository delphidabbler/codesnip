# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
#
# $Rev$
# $Date$
#
# Makefile for the CodeSnip project.


# Define macros giving relative paths to other directories from location of
# makefile
BIN = ..\Bin
LIBBIN = $(BIN)\Lib
PKGBIN = $(LIBBIN)\Pkg
EXE = ..\Exe

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

TLIBIMP = "$(DELPHIROOT)\Bin\TLibImp.exe"

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

!ifndef IGNOREMIDL
!ifdef MSSDK
MIDL = "$(MSSDK)\Bin\MIDL.exe"
!else
# MSSDK is required
!error MSSDK environment variable required.
!endif
!else
# MIDL.exe can be ignored if IGNOREMIDL macro is defined: replace with echo
MIDL = @echo *** IGNORING MIDL
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
everything: config libraries exes setup release

# Builds CodeSnip exe file + help
exes: codesnip help

# Builds libraries and CodeSnip exe
bin: libraries codesnip

# Build CodeSnip executable
codesnip: typelib resources autogen pascal

# Configure source folders
config:
  @echo Configuring CodeSnip
  # Create .cfg file from template
  @copy /Y CodeSnip.cfg.tplt CodeSnip.cfg
  # Create build folders
  @cd ..
  @if exist Bin del /Q Bin\*.*
  @if not exist Bin mkdir Bin
  @if not exist Exe mkdir Exe
  @if not exist Release mkdir Release
  @cd Src

# Builds CodeSnip pascal files and links program
pascal: CodeSnip.exe

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
CodeSnip.exe:
  @echo +++ Compiling Pascal +++
  @$(DCC32) $(@B).dpr -B -U"$(INDY10)" $(DELPHIDEFINES)

# Builds help file
help:
  -1 @$(HHC) .\Help\CodeSnip.chm

# Compiles the resources and deletes intermediate file created by VIED
VERINFOFILEBASE = VCodeSnip
resources: $(VERINFOFILEBASE).res Resources.res HTML.res
  -@del $(VERINFOFILEBASE).rc

# Compiles HTMLres from .hrc file
HTML.res: HTML.hrc
  @echo +++ Compiling HTML Resource manifest file +++
  @$(HTMLRES) -mHTML.hrc -o$(BIN)\HTML.res -r -q

# Compiles type library from IDL
typelib:
  @$(MIDL) .\ExternalObj.idl /tlb $(BIN)\ExternalObj.tlb /I "$(MSSDK)\Include"

# Builds setup program
setup:
  @del ..\Exe\CodeSnip-Setup-*
  @$(ISCC) Install\CodeSnip.iss

# Creates auto generated files
autogen:
  @$(TLIBIMP) -P+ -Ps+ -D.\AutoGen -FtIntfExternalObj $(BIN)\ExternalObj.tlb
  @if exist .\AutoGen\IntfExternalObj.dcr del .\AutoGen\IntfExternalObj.dcr

# Build release files (.zip)
!ifndef RELEASEFILENAME
RELEASEFILENAME = dd-codesnip
!endif
STANDARDOUTFILE = Release\$(RELEASEFILENAME).zip
PORTABLEOUTFILE = Release\$(RELEASEFILENAME)-portable.zip
release:
  @echo.
  @echo Creating Release Files
  @echo ----------------------
  @cd ..
  -@if exist $(STANDARDOUTFILE) del $(STANDARDOUTFILE)
  @$(ZIP) -j -9 $(STANDARDOUTFILE) Exe\CodeSnip-Setup-*.exe Docs\ReadMe.txt
  @$(ZIP) -j -9 $(PORTABLEOUTFILE) Exe\CodeSnip.exe
  @$(ZIP) -j -9 $(PORTABLEOUTFILE) Exe\CodeSnip.chm
  @$(ZIP) -j -9 $(PORTABLEOUTFILE) Docs\ReadMe.txt
  @$(ZIP) -j -9 $(PORTABLEOUTFILE) Docs\Privacy.txt
  @$(ZIP) -j -9 $(PORTABLEOUTFILE) Docs\License.html
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

# Make libraries
libraries: cfglib Collections.bpl SynEdit_RXE.bpl DDabLib.bpl

cfglib:
  @if not exist "$(BIN)" mkdir "$(BIN)"
  @if not exist "$(LIBBIN)" mkdir "$(LIBBIN)"
  @if not exist "$(PKGBIN)" mkdir "$(PKGBIN)"

Collections.bpl:
  @cd Lib\DelphiColl
  @$(DCC32) $(@B).dpk -B $(DELPHIDEFINES)
  @cd ..\..

SynEdit_RXE.bpl:
  @cd Lib\SynEdit
  @$(DCC32) $(@B).dpk -B $(DELPHIDEFINES)
  @cd ..\..

DDabLib.bpl:
  @cd Lib\DDabLib
  @$(DCC32) $(@B).dpk -B $(DELPHIDEFINES)
  @cd ..\..
