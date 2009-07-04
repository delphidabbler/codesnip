@rem ---------------------------------------------------------------------------
@rem Script used to build the DelphiDabbler CodeSnip Install Helper.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008.
@rem
@rem v1.0 of 14 Aug 2008 - First version.
@rem v1.1 of 24 Aug 2008 - Modified to work witrh Delphi 2006
@rem 
@rem
@rem Requires:
@rem   Borland Delphi 2006
@rem   Borland BRCC32 from Delphi 2006 installation
@rem   DelphiDabbler Version Information Editor from www.delphidabbler.com.
@rem
@rem Also requires the following environment variables:
@rem   DELPHI2006 to be set to the install directory of Delphi 2006.
@rem
@rem Switches: there are no recognised switches
@rem
@rem ---------------------------------------------------------------------------

@echo off

setlocal


rem ----------------------------------------------------------------------------
rem Sign on
rem ----------------------------------------------------------------------------

title DelphiDabbler CodeSnip Install Helper Build Script

echo DelphiDabbler CodeSnip Install Helper Build Script
echo --------------------------------------------------

rem ----------------------------------------------------------------------------
rem Check that required environment variables exist
rem ----------------------------------------------------------------------------

:CheckEnvVars

echo Checking predefined environment environment variables
if not defined DELPHI2006 goto BadDELPHIEnv
echo Done.
echo.
goto SetEnvVars

:BadDELPHIEnv
set ErrorMsg=DELPHI2006 Environment variable not defined
goto Error


rem ----------------------------------------------------------------------------
rem Set up required environment variables
rem ----------------------------------------------------------------------------

:SetEnvVars
echo Setting Up Environment
rem set up environment for MS SDK

rem directories

rem source directory
set SrcDir=.\
rem binary files directory
set BinDir=..\..\Bin\InstallHelper\
rem executable files directory
set ExeDir=..\..\Exe\

rem executable programs

rem Delphi 7 - use full path since maybe multple installations
set DCC32Exe="%DELPHI2006%\Bin\DCC32.exe"
rem Borland Resource Compiler - use full path since maybe multple installations
set BRCC32Exe="%DELPHI2006%\Bin\BRCC32.exe"
rem DelphiDabbler Version Information Editor - assumed to be on the path
set VIEdExe=VIEd.exe

echo Done.
echo.


rem ----------------------------------------------------------------------------
rem Start of build process
rem ----------------------------------------------------------------------------

:Build
echo BUILDING ...
echo.


rem ----------------------------------------------------------------------------
rem Build resource files
rem ----------------------------------------------------------------------------

echo Building Resources
echo.

rem set required env vars

rem Ver info resource
set VerInfoBase=VCSSetupHelper
set VerInfoSrc=%SrcDir%%VerInfoBase%.vi
set VerInfoTmp=%SrcDir%%VerInfoBase%.rc
set VerInfoRes=%BinDir%%VerInfoBase%.res
rem General resource
set ResourcesBase=CSSetupHelperRes
set ResourcesSrc=%SrcDir%%ResourcesBase%.rc
set ResourcesRes=%BinDir%%ResourcesBase%.res

rem Compile version information resource

echo Compiling %VerInfoSrc% to %VerInfoRes%
rem VIedExe creates temp resource .rc file from .vi file
set ErrorMsg=
%VIEdExe% -makerc %VerInfoSrc%
if errorlevel 1 set ErrorMsg=Failed to compile %VerInfoSrc%
if not "%ErrorMsg%"=="" goto VerInfoRes_End
rem BRCC32Exe compiles temp resource .rc file to required .res
%BRCC32Exe% %VerInfoTmp% -fo%VerInfoRes%
if errorlevel 1 set ErrorMsg=Failed to compile %VerInfoTmp%
if not "%ErrorMsg%"=="" goto VerInfoRes_End
echo Done
echo.

:VerInfoRes_End
if exist %VerInfoTmp% del %VerInfoTmp%
if not "%ErrorMsg%"=="" goto Error

rem Compile general resource

echo Compiling %ResourcesSrc% to %ResourcesRes%
%BRCC32Exe% %ResourcesSrc% -fo%ResourcesRes%
if errorlevel 1 goto ResourcesRes_Error
echo Done
echo.
goto ResourcesRes_End

:ResourcesRes_Error
set ErrorMsg=Failed to compile %ResourcesSrc%
goto Error

:ResourcesRes_End

rem End of resource compilation


rem ----------------------------------------------------------------------------
rem Build Pascal project
rem ----------------------------------------------------------------------------

rem Set up required env vars
set PascalBase=CSSetupHelper
set PascalSrc=%SrcDir%%PascalBase%.dpr
set PascalExe=%ExeDir%%PascalBase%.exe

echo Building Pascal Project
%DCC32Exe% -B %PascalSrc%
if errorlevel 1 goto Pascal_Error
goto Pascal_End

:Pascal_Error
set ErrorMsg=Failed to compile %PascalSrc%
if exist %PascalExe% del %PascalExe%
goto Error

:Pascal_End
echo Done.
echo.

rem End of Pascal compilation


rem ----------------------------------------------------------------------------
rem Build completed
rem ----------------------------------------------------------------------------

:Build_End
echo BUILD COMPLETE
echo.

goto End


rem ----------------------------------------------------------------------------
rem Handle errors
rem ----------------------------------------------------------------------------

:Error
echo.
echo *** ERROR: %ErrorMsg%
echo.


rem ----------------------------------------------------------------------------
rem Finished
rem ----------------------------------------------------------------------------

:End
echo.
echo DONE.
endlocal
