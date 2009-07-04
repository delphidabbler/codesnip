@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing source code of CodeSnip
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2005-2008
@rem
@rem v1.0 of 08 Jun 2006 - First version.
@rem v1.1 of 14 Aug 2008 - Added support for CSSetupHelper:
@rem                       - Deleted CSSetupHelper.dsk
@rem                       - Copy .res files in Bin\InstallHelper
@rem                     - Changed to use setlocal and endlocal
@rem ---------------------------------------------------------------------------

@echo off

setlocal

cd ..

set OutFile=Release\dd-codesnip-src.zip
del %OutFile%

zip -r -9 %OutFile% Src
zip -r -9 %OutFile% Bin\*.res
zip -r -9 %OutFile% Bin\*.tlb
zip -r -9 %OutFile% Bin\InstallHelper\*.res
zip  -j -9 %OutFile% Docs\SourcecodeLicenses.txt Docs\ReadMe-src.txt Docs\MPL.txt
zip %OutFile% -d Src\CodeSnip.dsk
zip %OutFile% -d Src\InstallHelper\CSSetupHelper.dsk

endlocal