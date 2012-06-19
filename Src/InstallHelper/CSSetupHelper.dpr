{
 * CSSetupHelper.dpr
 *
 * Helper program that is called from CodeSnip's installation program. Gets the
 * original user's application data folder and writes it to a file that can be
 * read by install program.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is CSSetupHelper.dpr
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *    NONE
 *
 * ***** END LICENSE BLOCK *****
}



program CSSetupHelper;


{$ALIGN 8}
{$APPTYPE CONSOLE}
{$BOOLEVAL ON}
{$DESCRIPTION 'CodeSnip Install Helper'}
{$EXTENDEDSYNTAX ON}
{$HINTS ON}
{$IOCHECKS ON}
{$LONGSTRINGS ON}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$TYPEDADDRESS OFF}
{$WARNINGS ON}
{$WRITEABLECONST OFF}


{
  NOTE:

  This program gets the path to the application data folder for the current
  user and writes it to a file.

  The file is a text file in the INI file format. There is only one section,
  named SpecialFolders. The path to the application data folder is written to
  the only key in the section, named CSIDL_APPDATA.

  This format can obviously be extended with other user specific special
  folders.
}


{$R CSSetupHelperRes.res}   // general resources
{$R VCSSetupHelper.res}     // version information


uses
  // Delphi
  Windows, ShlObj, ActiveX;


procedure FreePIDL(PIDL: PItemIDList);
  {Uses to shell allocator to free the memory used by a PIDL.
    @param PIDL [in] PIDL that is to be freed.
  }
var
  Malloc: IMalloc;  // shell's allocator
begin
  if Succeeded(SHGetMalloc(Malloc)) then
    Malloc.Free(PIDL);
end;

function PIDLToFolderPath(PIDL: PItemIDList): string;
  {Returns the full path to a file system folder described by a PIDL.
    @param PIDL [in] PIDL describing folder.
    @return Full path to folder described by PIDL or '' if PIDL refers to
      virtual folder.
  }
begin
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIDList(PIDL, PChar(Result)) then
    Result := PChar(Result)
  else
    Result := '';
end;

function SpecialFolderPath(CSIDL: Integer): string;
  {Returns the full path to a special file system folder.
    @param CSIDL [in] Specifies the special folder.
    @return Folder path or '' if the special folder is virtual or CSIDL not
      supported on the OS.
  }
var
  PIDL: PItemIDList;  // PIDL of the special folder
begin
  Result := '';
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, PIDL)) then
  begin
    try
      Result := PIDLToFolderPath(PIDL);
    finally
      FreePIDL(PIDL);
    end;
  end
end;

var
  TempFile: string;     // temporary file user to store data
  AppDataDir: string;   // user's application data folder
begin
  // Get file we are to write from command line (only param)
  TempFile := ParamStr(1);
  // Get user's application data folder and write it to temp file in ini format
  AppDataDir := SpecialFolderPath(CSIDL_APPDATA);
  WritePrivateProfileString(
    'SpecialFolders',
    'CSIDL_APPDATA',
    PChar(AppDataDir),
    PChar(TempFile)
  );
end.

