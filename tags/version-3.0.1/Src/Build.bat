@rem ---------------------------------------------------------------------------
@rem Script used to build the DelphiDabbler CodeSnip project
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2006-2008.
@rem
@rem v1.0 of 06 Jun 2006 - First version that builds all project
@rem v1.1 of 21 Apr 2008 - Removed call to MS SDK's SetEnv.cmd and instead
@rem                       directly set path to required directories to get MIDL
@rem                       to work.
@rem                     - Changed to worj with single Resources.rc file instead
@rem                       of Images.rc and Dialogs.rc.
@rem                     - Removed some redundant code.
@rem                     - Now set (renamed) window title at top of file.
@rem v1.2 of 05 Jun 2008 - Fixed error in environment variable setting code.
@rem v1.3 of 24 Aug 2008 - Modified to work with Delphi 2006.
@rem                     - Disabled output from HTMLRes
@rem 
@rem
@rem Requires:
@rem   Borland Delphi 2006
@rem   Borland BRCC32 from Delphi 2006 installation
@rem   Borland TLibImp from Delphi 2006 installation
@rem   Microsoft MIDL.exe from Platform SDK (which requires Visual C++'s CL.exe
@rem     C pre-processor which in turn requires mspdb**.dll). This file was
@rem     tested with the Windows 2008 (v6.1) version of the Platform SDK which
@rem     comes with files from Visual Studio 9.
@rem   DelphiDabbler HTML Resource Compiler from www.delphidabbler.com
@rem   DelphiDabbler Version Information Editor from www.delphidabbler.com.
@rem   Inno Setup v5.2.3 or later with ISPP pre-processor v5.2.3 or later from
@rem     http://www.innosetup.com/.
@rem
@rem Also requires the following environment variables:
@rem   DELPHI2006 to be set to the install directory of Delphi 2006.
@rem   DELPHIDABLIBD2006 to be set to the install directory of the required
@rem     DelphiDabbler components on Delphi 2006.
@rem   INDY9 to be set to the folder containing the Delphi 2006 Indy 9
@rem     components.
@rem   MSSDK to be set to Microsoft Platform SDK install directory. MIDL.exe
@rem     will be in Bin sub-directory and include files in Include sub-
@rem     directory.
@rem   MSCPP to be set to the path to the MS cl.exe C pre-processor. This will
@rem    probably be in a sub folder of a MS Visual Studio installation folder.
@rem   MSCOMMONIDE to be set to Common Visual studio binary directory where
@rem     mspdb**.dll is located (required by cl.exe).
@rem   INNOSETUP to be set to Inno Setup install directory.
@rem
@rem Switches: exactly one of the following must be provided
@rem   all - build everything
@rem   res - build binary resource files only
@rem   tlb - build type library from IDL and associated .pas file
@rem   pas - build Delphi Pascal project only
@rem   help - build help file
@rem   setup - build setup file
@rem
@rem ---------------------------------------------------------------------------

@echo off

setlocal


rem ----------------------------------------------------------------------------
rem Sign on
rem ----------------------------------------------------------------------------

title DelphiDabbler CodeSnip Build Script

echo DelphiDabbler CodeSnip Build Script
echo -----------------------------------

goto Config

rem ----------------------------------------------------------------------------
rem Configure script per command line parameter
rem ----------------------------------------------------------------------------

:Config
echo Configuring script
rem reset all config variables

set BuildAll=
set BuildResources=
set BuildTypeLib=
set BuildPascal=
set BuildHelp=
set BuildSetup=

rem check switch

if "%~1" == "all" goto Config_BuildAll
if "%~1" == "res" goto Config_BuildResources
if "%~1" == "tlb" goto Config_BuildTypeLib
if "%~1" == "pas" goto Config_BuildPascal
if "%~1" == "help" goto Config_BuildHelp
if "%~1" == "setup" goto Config_BuildSetup
set ErrorMsg=Unknown switch "%~1"
if "%~1" == "" set ErrorMsg=No switch specified
goto Error

rem set config variables

:Config_BuildAll
set BuildResources=1
set BuildTypeLib=1
set BuildPascal=1
set BuildHelp=1
set BuildSetup=1
goto Config_OK

:Config_BuildResources
set BuildResources=1
goto Config_OK

:Config_BuildTypeLib
set BuildTypeLib=1
goto Config_OK

:Config_BuildPascal
set BuildPascal=1
goto Config_OK

:Config_BuildHelp
set BuildHelp=1
goto Config_OK

:Config_BuildSetup
set BuildSetup=1
goto Config_OK

rem script configured OK

:Config_OK
echo Done.
goto CheckEnvVars


rem ----------------------------------------------------------------------------
rem Check that required environment variables exist
rem ----------------------------------------------------------------------------

:CheckEnvVars

echo Checking predefined environment environment variables
if not defined DELPHI2006 goto BadDELPHIEnv
if not defined DELPHIDABLIBD2006 goto BadDELPHIDABLIBEnv
if not defined INDY9 goto BadIndyEnv
if not defined MSSDK goto BadMSSDKEnv
if not defined MSCPP goto BadMSCPPEnv
if not defined MSCOMMONIDE goto BadMSCOMMONIDEEnv
if not defined INNOSETUP goto BadINNOSETUPEnv
echo Done.
echo.
goto SetEnvVars

rem we have at least one undefined env variable

:BadDELPHIEnv
set ErrorMsg=DELPHI2006 Environment variable not defined
goto Error

:BadDELPHIDABLIBEnv
set ErrorMsg=DELPHIDABLIBD2006 Environment variable not defined
goto Error

:BadIndyEnv
set ErrorMsg=INDY9 Environment variable not defined
goto Error

:BadMSSDKEnv
set ErrorMsg=MSSDK Environment variable not defined
goto Error

:BadMSCPPEnv
set ErrorMsg=MSCPP Environment variable not defined
goto Error

:BadMSSDKINCEnv
set ErrorMsg=MSSDKINC Environment variable not defined
goto Error

:BadMSCOMMONIDEEnv
set ErrorMsg=MSCOMMONIDE Environment variable not defined
goto Error

:BadINNOSETUPEnv
set ErrorMsg=INNOSETUP Environment varibale not defined
goto Error


rem ----------------------------------------------------------------------------
rem Set up required environment variables
rem ----------------------------------------------------------------------------

:SetEnvVars
echo Setting Up Environment
rem set up environment for MS SDK

rem And add a MSCPP and MSCOMMONIDE to Path otherwise we can't find a C 
rem pre-processor
set PATH=%MSCPP%;%MSCOMMONIDE%;%PATH%

rem directories

rem full path to this file
set BuildDir=%~dp0
rem source directory
set SrcDir=.\
rem auto-generated source directory
set AutoSrcDir=%SrcDir%AutoGen\
rem help source directory
set HelpSrcDir=%SrcDir%Help\
rem install script source directory
set InstallSrcDir=%SrcDir%Install\
rem binary files directory
set BinDir=..\Bin\
rem executable files directory
set ExeDir=..\Exe\
rem install folder for Indy 9 component units
set IndyLib=%INDY9%
rem install folder for DelphiDabbler library units
set DDabLib=%DELPHIDABLIBD2006%

rem executable programs

rem Delphi 7 - use full path since maybe multple installations
set DCC32Exe="%DELPHI2006%\Bin\DCC32.exe"
rem Borland Resource Compiler - use full path since maybe multple installations
set BRCC32Exe="%DELPHI2006%\Bin\BRCC32.exe"
rem Borland Type Library Importer - full path since maybe multple installations
set TLibImpExe="%DELPHI2006%\Bin\TLibImp.exe"
rem Microsoft IDL Compiler
set MIDLExe="%MSSDK%\Bin\MIDL.exe"
rem Inno Setup command line compiler
set ISCCExe="%INNOSETUP%\ISCC.exe"
rem Microsoft HTML Workshop - assumed to be on the path
set HHCExe=HHC.exe
rem DelphiDabbler Version Information Editor - assumed to be on the path
set VIEdExe=VIEd.exe
rem DelphiDabbler HTML Resource Compiler - assumed to be on the path
set HTMLResExe=HTMLRes.exe

echo Done.
echo.

rem ----------------------------------------------------------------------------
rem Start of build process
rem ----------------------------------------------------------------------------

:Build
echo BUILDING ...
echo.

goto Build_Resources


rem ----------------------------------------------------------------------------
rem Build resource files
rem ----------------------------------------------------------------------------

:Build_Resources
if not defined BuildResources goto Build_TypeLib
echo Building Resources
echo.

rem set required env vars

rem Ver info resource
set VerInfoBase=VCodeSnip
set VerInfoSrc=%SrcDir%%VerInfoBase%.vi
set VerInfoTmp=%SrcDir%%VerInfoBase%.rc
set VerInfoRes=%BinDir%%VerInfoBase%.res
rem HTML resource
set HTMLBase=HTML
set HTMLSrc=%SrcDir%%HTMLBase%.hrc
set HTMLRes=%BinDir%%HTMLBase%.res
rem Images resource
set ResourcesBase=Resources
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

rem Compile HTML resource

echo Compiling %HTMLSrc% to %HTMLRes%
%HTMLResExe% -m%HTMLSrc% -o%HTMLRes% -r -q
if errorlevel 1 goto HTMLRes_Error
echo Done
echo.
goto HTMLRes_End

:HTMLRes_Error
set ErrorMsg=Failed to compile %HTMLSrc%
goto Error

:HTMLRes_End

rem Compile Resources resource

echo Compiling %ResourcesSrc% to %ResourcesRes%
%BRCC32Exe% %ResourcesSrc% -fo%ResourcesRes%
if errorlevel 1 goto ResourcesRes_Error
echo Done
echo.
goto ResourcesRes_End

:ResourcesRes_Error
set ErrorMsg=Failed to compile %ImagesSrc%
goto Error

:ResourcesRes_End

rem End of resource compilation

goto Build_TypeLib


rem ----------------------------------------------------------------------------
rem Build type library and auto-generated Pascal file
rem ----------------------------------------------------------------------------

:Build_TypeLib
if not defined BuildTypeLib goto Build_Pascal
echo Building Type Library
echo.

rem set required env vars
set TypeLibBase=ExternalObj
set TypeLibGenBase=Intf%TypeLibBase%
set TypeLibGenDir=%AutoSrcDir%
set TypeLibIdl=%SrcDir%%TypeLibBase%.idl
set TypeLibTlb=%BinDir%%TypeLibBase%.tlb
set TypeLibGenPas=%AutoSrcDir%%TypeLibGenBase%.pas
set TypeLibGenDcr=%AutoSrcDir%%TypeLibGenBase%.dcr
set TypeLibGenBak=%AutoSrcDir%%TypeLibGenBase%.tmp
set TypeLibGenHdr=%AutoSrcDir%%TypeLibGenBase%.hdr
set MIDLIncDir="%MSSDK%\Include"

rem compile type library from IDL
echo Compiling %TypeLibIdl% to %TypeLibTlb%
set ErrorMsg=
%MIDLExe% %TypeLibIdl% /tlb %TypeLibTlb% /I %MIDLIncDir%
if errorlevel 1 set ErrorMsg=Failed to compile %TypeLibIdl%
if not "%ErrorMsg%"=="" goto TypeLibIdl_End
echo Done
echo.

rem Create Pascal source file from TLB file

echo Creating %TypeLibGenPas% from %TypeLibTlb%
rem TLibImp creates .pas file and unwanted .dcr file (deleted later in script)
%TLibImpExe% -P+ -Ps+ -D%TypeLibGenDir% -Ft%TypeLibGenBase% %TypeLibTlb%
if errorlevel 1 set ErrorMsg=Failed to compile %TypeLibIdl%
if not "%ErrorMsg%"=="" goto TypeLibIdl_End
rem we prepend .hdr file to generated .pas file to add comments and license
rem we copy .pas file to .tmp then recreate .pas from .hdr and .tmp
echo Modifying %TypeLibGenPas%
if exist %TypeLibGenBak% del %TypeLibGenBak%
copy %TypeLibGenPas% %TypeLibGenBak% /Y >nul
copy %TypeLibGenHdr% + %TypeLibGenBak% %TypeLibGenPas% /Y >nul
echo Done
echo.

:TypeLibIdl_End
if exist %TypeLibGenDcr% del %TypeLibGenDcr%
if exist %TypeLibGenBak% del %TypeLibGenBak%
if not "%ErrorMsg%"=="" goto Error

rem End of type library compilation

goto Build_Pascal


rem ----------------------------------------------------------------------------
rem Build Pascal project
rem ----------------------------------------------------------------------------

:Build_Pascal
if not defined BuildPascal goto Build_Help

rem Set up required env vars
set PascalBase=CodeSnip
set PascalSrc=%SrcDir%%PascalBase%.dpr
set PascalExe=%ExeDir%%PascalBase%.exe

if not defined BuildPascal goto Build_Help
echo Building Pascal Project
%DCC32Exe% -B %PascalSrc% -U"%DDabLib%" -U"%IndyLib%"
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

goto Build_Help


rem ----------------------------------------------------------------------------
rem Build help project
rem ----------------------------------------------------------------------------

:Build_Help
if not defined BuildHelp goto Build_Setup
echo Building Help Project

rem Set required environment variables
set HelpBase=CodeSnip
set HelpChm=%ExeDir%%HelpBase%.chm
set HelpPrj=%HelpSrcDir%%HelpBase%.hhp

rem Compile help file: HHC returns 0 on failure, +ve on success
%HHCExe% %HelpPrj%
if errorlevel 1 goto Help_End
rem if we get here we have error
set ErrorMsg=Failed to compile %HelpPrj%
if exist %HelpChm% del %HelpChm%
goto Error

:Help_End
echo Done.
echo.

goto Build_Setup


rem ----------------------------------------------------------------------------
rem Build setup program
rem ----------------------------------------------------------------------------

:Build_Setup
if not defined BuildSetup goto Build_End
echo Building Setup Program

rem Set required environment variables
set SetupSrc=%InstallSrcDir%CodeSnip.iss
set SetupExeWild=%ExeDir%CodeSnip-Setup-*

echo SRC - %SetupSrc%
echo EXE - %SetupExeWild%

rem ISCC does not return error code if compile fails so find another way to
rem detect errors

del %SetupExeWild%
%ISCCExe% %SetupSrc%
if exist %SetupExeWild% goto Setup_End
set ErrorMsg=Failed to compile %SetupSrc%
goto Error

:Setup_End
echo Done.
echo.

goto Build_End


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

rem **** TO DO Unset all variables? ****

:End
echo.
echo DONE.
endlocal
