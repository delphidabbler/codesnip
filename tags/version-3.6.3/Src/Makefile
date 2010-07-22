# ------------------------------------------------------------------------------
# Makefile
#
# Makefile for the whole project.
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
# The Original Code is Makefile
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


# Set bin path: required by Common.mak
BIN = ..\Bin


!include Common.mak

# Define macros for all tools that are not common

TLIBIMP = "$(DELPHIROOT)\Bin\TLibImp.exe"

!ifdef HTMLRESROOT
HTMLRES = "$(HTMLRESROOT)\HTMLRes.exe"
!else
HTMLRES = HTMLRes
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

# Default is to build everything and release
everything: config exes setup release

# Builds all exe files + help
exes: codesnip installhelper help

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
  @if exist Exe rmdir /S /Q Exe
  @if exist Release rmdir /S /Q Release
  @mkdir Bin
  @mkdir Bin\InstallHelper
  @mkdir Exe
  @mkdir Release
  @cd Src
  # Config install helper
  @cd InstallHelper
  @$(MAKE) config
  @cd ..

# Build install helper executable
installhelper: 
  @cd InstallHelper
  @$(MAKE) exe
  @cd ..

# Builds CodeSnip pascal files and links program
pascal: CodeSnip.exe

# Builds help file
help:
  -1 @$(HHC) .\Help\CodeSnip.chm

# Compiles the resources and deletes intermediate file created by VIED
resources: VCodeSnip.res Resources.res HTML.res
  -@del VCodeSnip.rc

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
OUTFILE = Release\$(RELEASEFILENAME).zip
release:
  @echo.
  @echo Creating Release File
  @echo ---------------------
  @cd ..
  -@if exist $(OUTFILE) del $(OUTFILE)
  @$(ZIP) -j -9 $(OUTFILE) Exe\CodeSnip-Setup-*.exe Docs\ReadMe.txt
  @cd Src

# Clean up unwanted files
clean:
  @cd ..
  # remove unwanted files: temps (~ or .~*), .dsk, .local, .identcache
  -@del /S *.~* 2>nul
  -@del /S ~* 2>nul
  -@del /S *.dsk 2>nul
  -@del /S *.local 2>nul
  -@del /S *.identcache 2>nul
  # remove __history folders
  -@for /F "usebackq" %i in (`dir /S /B /A:D ..\__history`) do @rmdir /S /Q %i
  @cd Src
