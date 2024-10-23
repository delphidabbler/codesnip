:: This Source Code Form is subject to the terms of the Mozilla Public License,
:: v. 2.0. If a copy of the MPL was not distributed with this file, You can
:: obtain one at https://mozilla.org/MPL/2.0/
::
:: Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
::
:: Deploy script for CodeSnip.
::
:: This script compiles release versions of the standard and portable editions
:: of CodeSnip and places them into two different zip files ready for release.
::
:: This script uses Embarcadero Make. Various other programs are required to
:: run Make. See Src/Makefile for details.
::
:: To use the script:
::   1) Set the environment variables required for Make to succeed. See 
::      Src/Makefile for details
::   2) Change directory to that where this script is located.
::   3) Run the script.
::
:: Usage:
::   Deploy <version>
:: where
::   <version> is the version number of the release, e.g. 0.5.3-beta or 1.2.0.

@echo off

setlocal

:: Check for required parameter
if "%1"=="" goto paramerror

:: Store parameter
set Version=%1

:: Store common make parameters
set CommonParams=-DVERSION=%Version%

:: Store standard edition make parameters
set StandardParams=%CommonParams%

:: Store portable edition make parameters
set PortableParams=-DPORTABLE %CommonParams%

:: Set command line
set MakeCmd=Make
set StandardMakeCmd=%MakeCmd% %StandardParams%
set PortableMakeCmd=%MakeCmd% %PortableParams%

echo ----------------------------------------------
echo Deploying CodeSnip Standard And Portable Builds
echo -----------------------------------------------
echo.
echo Standard edition Make command: %StandardMakeCmd%
echo Portable edition Make command: %PortableMakeCmd%

cd Src

echo.
echo.
echo.
echo =========================
echo Building Standard edition
echo =========================
echo.
echo.
%StandardMakeCmd%

echo.
echo.
echo.
echo =========================
echo Building Portable edition
echo =========================
echo.
echo.
%PortableMakeCmd%

echo.
echo.
echo.
echo ====================
echo Deployment completed
echo ====================

goto end

:: Error messages

:paramerror
echo.
echo ***ERROR: Please specify a version number as a parameter
echo.
goto end

:: End
:end

endlocal
