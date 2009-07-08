@rem ---------------------------------------------------------------------------
@rem Script used to create a backup file of the CodeSnip development tree.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2009
@rem
@rem v1.0 of 08 Jul 2009 - First version.
@rem
@rem Usage: Backup <path-to-backup>\<backup-zip-file>
@rem ---------------------------------------------------------------------------

@echo off

setlocal

call Tidy.bat

cd ..

set OutFile="%1"
del %OutFile%

zip -r -9 %OutFile% Bin
zip -r -9 %OutFile% DevTools
zip -r -9 %OutFile% Docs
zip -r -9 %OutFile% Exe
zip -r -9 %OutFile% Src

endlocal
