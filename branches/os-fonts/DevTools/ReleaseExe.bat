@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing binary release of CodeSnip
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2005-2006
@rem
@rem v1.0 of 08 Jun 2006 - First version.
@rem ---------------------------------------------------------------------------

@echo off
del ..\release\dd-codesnip.zip
zip -j -9 ..\release\dd-codesnip.zip ..\Exe\CodeSnip-Setup-*.exe ..\Docs\ReadMe.txt
