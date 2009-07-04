@rem ---------------------------------------------------------------------------
@rem Script used to delete temp and backup source files
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2006-2009
@rem
@rem v1.0 of 09 Jun 2006 - First version.
@rem v2.0 of 21 Apr 2008 - Complete rewrite: now recursively deletes each file
@rem                       type from source and all subfolders.
@rem v2.1 of 24 Aug 2008 - Added code to remove __history folders from Src and
@rem                       Src\InstallHelper directories.
@rem v2.2 of 10 May 2009 - Added code to removed __history folders from all Src
@rem                       directories except those with images or no files.
@rem ---------------------------------------------------------------------------

@echo off
set SrcDir=..\Src

echo Deleting *.~* from "%SrcDir%" and subfolders
del /S %SrcDir%\*.~* 
echo.

echo Deleting *.dpp from "%SrcDir%" and subfolders
del /S %SrcDir%\*.ddp 
echo.

echo Deleting __history directories
rmdir %SrcDir%\__history
rmdir %SrcDir%\3rdParty\__history
rmdir %SrcDir%\AutoGen\__history
rmdir %SrcDir%\Help\__history
rmdir %SrcDir%\Help\CSS\__history
rmdir %SrcDir%\Help\HTML\__history
rmdir %SrcDir%\Install\__history
rmdir %SrcDir%\Res\HTML\__history
rmdir %SrcDir%\Res\Misc\__history
rmdir %SrcDir%\InstallHelper\__history
rmdir %SrcDir%\InstallHelper\Res\__history


echo Done.
