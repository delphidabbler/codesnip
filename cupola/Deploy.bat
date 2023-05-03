:: Deploy script for CodeSnip LE Cupola.
::
:: This script compiles the release version of the 64 and 32 bit builds of
:: CodeSnip LE and places them into a single zip file ready for release.
::
:: This script uses MSBuild and InfoZip's zip.exe. The MSBuild project also
:: requires DelphiDabbler Version Information Editor.
::
:: Get zip.exe from https://delphidabbler.com/extras/info-zip
:: Get Version Information Editor from https://delphidabbler.com/software/vied

:: To use the script:
::   1) Start the Embarcadero RAD Studio Command Prompt to set the required
::      environment variables for MSBuild.
::   2) Set the ZIPROOT environment variable to the directory where zip.exe is
::      installed.
::   3) Set the VIEDROOT environment variable to the directory where VIEd.exe is
::      installed.
::   3) Change directory to that where this script is located.
::   4) Run the script.
::
:: Usage:
::   Deploy <version>
:: where
::   <version> is the version number of the release, e.g. 0.5.3-beta or 1.2.0.

@echo off

echo -----------------------------
echo Deploying CodeSnip LE Release
echo -----------------------------

:: Check for required parameter
if "%1"=="" goto paramerror

:: Check for required environment variables
if "%ZipRoot%"=="" goto envvarerror
if "%VIEdRoot"=="" goto envvarerror

:: Set variables
set Version=%1
set BuildRoot=.\_build
set AppBuildRoot=%BuildRoot%\app
set Win32Dir=%AppBuildRoot%\Win32\Release\exe
set Win64Dir=%AppBuildRoot%\Win64\Release\exe
set ReleaseDir=%BuildRoot%\_release
set OutFile=%ReleaseDir%\codesnip-le-cupola-%Version%.zip
set SrcDir=src
set DocsDir=docs
set ProjectName=CodeSnip.Cupola
set Exe32=%ProjectName%32.exe
set Exe64=%ProjectName%.exe
set ReadMeFile=INSTALLATION-NOTES.txt
set ZipExe="%ZipRoot%\zip.exe"

:: Make a clean directory structure
if exist %BuildRoot% rmdir /S /Q %BuildRoot%
mkdir %ReleaseDir%

setlocal

:: Build Pascal
cd %SrcDir%

echo.
echo Building 32 bit version
echo.
msbuild %ProjectName%.dproj /p:config=Release /p:platform=Win32
echo.

echo.
echo Building 64 bit version
echo.
msbuild %ProjectName%.dproj /p:config=Release /p:platform=Win64
echo.

endlocal

:: Rename 32 bit exe files with "32" suffix
setlocal
cd %Win32Dir%
ren %ProjectName%.exe %Exe32%
endlocal

:: Create zip files
echo.
echo Creating zip files
%ZipExe% -j -9 %OutFile% %Win32Dir%\%EXE32%
%ZipExe% -j -9 %OutFile% %Win64Dir%\%EXE64%
%ZipExe% -j -9 %OutFile% %DocsDir%\%ReadMeFile%

echo.
echo ---------------
echo Build completed
echo ---------------

goto end

:: Error messages

:paramerror
echo.
echo ***ERROR: Please specify a version number as a parameter
echo.
goto end

:envvarerror
echo.
echo ***ERROR: ZipRoot and/or VIEdRoot environment variable not set
echo.
goto end

:: End
:end
