# Makefile for the CodeSnip DUnit tests
# -------------------------------------
#
# $Rev$
# $Date$


# Define macros giving relative paths to other directories from location of
# makefile
BIN = .\Bin
EXE = .\Exe

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

!if ! $d(DUNIT)
DUNIT = $(DELPHIROOT)\Source\DUnit\src
!endif

# Command line options
DELPHIDEFINES = -DTESTING
!if $d(CONSOLE)
DELPHIDEFINES = $(DELPHIDEFINES) -DCONSOLE_TESTRUNNER
!endif

# Default is to configure and build
everything: config pascal

# Configure source folders
config:
  @echo Configuring CodeSnip
  @if exist $(BIN) del /Q $(BIN)\*.*
  @if not exist $(BIN) mkdir $(BIN)
  @if not exist $(EXE) mkdir $(EXE)

# Builds CodeSnipTest pascal files and links program
pascal: CodeSnipTests.exe

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
CodeSnipTests.exe:
  @echo +++ Compiling Pascal +++
  $(DCC32) $(@B).dpr -B -U"$(DUNIT)" $(DELPHIDEFINES)
