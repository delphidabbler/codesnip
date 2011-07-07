{
 * UUtils.pas
 *
 * General utility routines.
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
 * The Original Code is UUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUtils;


interface


uses
  // Delphi
  SysUtils, Classes;


function FileAge(const FileName: string): Integer;
  {Gets the OS time stamp for a file.
    @param FileName [in] Name of file.
    @return Required OS time stamp or -1 if file does not exist.
  }

function DeleteFiles(const Dir, Wildcard: string): Integer;
  {Deletes all files in a directory that match a wildcard. Sub-directories are
  not deleted.
    @param Dir [in] Directory containing files to be deleted.
    @param Wildcard [in] Wildcard specifying files to be deleted - *.* assumed
      if ''.
    @return Number of files deleted.
  }

procedure EnsureFolders(const Folder: string);
  {Ensures that a folder and all its subfolder exist.
    @param Folder [in] Fully specified name of folder.
  }

function ListFiles(const Dir, Wildcard: string; const List: TStrings;
  IncludeDirs: Boolean = True): Boolean;
  {Gets a list of the files and sub-directories of a directory that match a
  wild card. The file names included in the list include the full file path
    @param Dir [in] Directory to be listed.
    @param Wildcard [in] Wildcard of files to be listed.
    @param List [in] Receives directory listing.
    @param IncludeDirs [in] Flag true if sub-directory names to be included in
      List, False if true files only are required.
    @return True if Dir is a valid directory.
  }

function LongToShortFilePath(const LongName: string): string;
  {Converts a long file name to the equivalent shortened DOS style 8.3 path.
    @param LongName [in] Long file name to be converted.
    @return Short file name.
  }

function IsDirectory(const DirName: string): Boolean;
  {Checks if a directory exists.
    @param DirName [in] Name of directory to check.
    @return True if DirName is valid directory.
  }

function FloatToInt(const F: Double): Int64;
  {Converts a floating point number to an integer, rounding to nearest integer.
    @param F [in] Floating point number to be rounded off.
    @return Rounded value as integer.
  }

function DateStamp: string;
  {Creates a date stamp in RFC1123 format
    @return Current date and time as date stamp in UTC/GMT.
  }

procedure GetIntf(const Instance: IInterface; const IID: TGUID; out Intf);
  {Get a desired interface pointer to an object instance.
    @param Instance [in] IInterface of instance for which an interface is
      requested. May be nil.
    @param IID [in] Identifier of required interface.
    @param Intf [out] Set to required interface pointer if Instance supports
      interface, or nil if interface not supported or Instance is nil.
  }

function IsBaseFileName(const FileName: string): Boolean;
  {Checks if a file name is a base file name (i.e. contains no path
  information).
    @param FileName [in] File name to be tested.
    @return True if file is a base file name, False otherwise.
  }

procedure Pause(const ADelay: Cardinal);
  {Pauses for a specified number of milliseconds before returning. Performs a
  busy wait.
    @param ADelay [in] Number of milliseconds to pause.
  }

function IsValidDriveLetter(const C: Char): Boolean;
  {Checks if a character is a valid Windows drive letter.
    @param C [in] Character to be tested.
    @return True if C is a valid drive letter, False otherwise.
  }

procedure KeyErrorBeep;
  {Emits a sound indicating a keypress error.
  }

function IsHexDigit(C: Char): Boolean;
  {Checks whether a character is defined as a hex digit.
    @param C [in] Character to be tested.
    @return True if character is a hex digit, False if not.
  }

// todo: comment this routine
function URIBaseName(const URI: string): string;


implementation


uses
  // Delphi
  Windows, ShlObj, ActiveX, Messages, Character, Math,
  // Project
  UConsts, UStrUtils;


function FileAge(const FileName: string): Integer;
  {Gets the OS time stamp for a file.
    @param FileName [in] Name of file.
    @return Required OS time stamp or -1 if file does not exist.
  }
var
  FH: Integer;  // file handle
begin
  // This function is provided to avoid using FileAge unit in SysUtils since
  // the routine is deprecated in Delphi 2006
  Result := -1;
  if IsDirectory(FileName) then
    Exit;
  FH := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if FH <> -1 then
  begin
    Result := FileGetDate(FH);
    FileClose(FH);
  end;
end;

function DeleteFiles(const Dir, Wildcard: string): Integer;
  {Deletes all files in a directory that match a wildcard. Sub-directories are
  not deleted.
    @param Dir [in] Directory containing files to be deleted.
    @param Wildcard [in] Wildcard specifying files to be deleted - *.* assumed
      if ''.
    @return Number of files deleted.
  }
var
  Files: TStringList; // stores files to be deleted
  I: Integer;         // loops thru files in folder
  AFile: string;      // a file to be deleted
  Attr: Integer;      // attributes of a file
begin
  Result := 0;
  Files := TStringList.Create;
  try
    // Get matching list of files / folders in directory
    if not ListFiles(Dir, Wildcard, Files) then
      Exit;
    for I := 0 to Pred(Files.Count) do
    begin
      AFile := Files[I];
      Attr := FileGetAttr(AFile);
      // Delete file if it is not a directory
      if (Attr and faDirectory = 0) then
      begin
        if SysUtils.DeleteFile(AFile) then
          // File deleted: count it
          Inc(Result);
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
end;

procedure EnsureFolders(const Folder: string);
  {Ensures that a folder and all its subfolder exist.
    @param Folder [in] Fully specified name of folder.
  }
begin
  // Check there's a folder to create: ForceDirectories raises exception if
  // passed empty string as parameter
  if Length(Folder) = 0 then
    Exit;
  // Create the folders
  ForceDirectories(Folder);
end;

function IsDirectory(const DirName: string): Boolean;
  {Checks if a directory exists.
    @param DirName [in] Name of directory to check.
    @return True if DirName is valid directory.
  }
begin
  Result := DirectoryExists(DirName);
end;

function ListFiles(const Dir, Wildcard: string; const List: TStrings;
  IncludeDirs: Boolean = True): Boolean;
  {Gets a list of the files and sub-directories of a directory that match a
  wild card. The file names included in the list include the full file path
    @param Dir [in] Directory to be listed.
    @param Wildcard [in] Wildcard of files to be listed.
    @param List [in] Receives directory listing.
    @param IncludeDirs [in] Flag true if sub-directory names to be included in
      List, False if true files only are required.
    @return True if Dir is a valid directory.
  }
var
  FileSpec: string;   // full file spec of a wildcard
  Path: string;       // full path of directory, including training backslash
  SR: TSearchRec;     // file search result
  Success: Integer;   // success code for FindXXX routines
const
  faVolumeId = $00000008; // redefined from SysUtils to avoid deprecated warning
begin
  Assert(Assigned(List), 'ListFiles: List is nil');
  // Check if true directory and exit if not
  Result := IsDirectory(Dir);
  if not Result then
    Exit;
  // Build FileSpec from directory and wildcard
  FileSpec := IncludeTrailingPathDelimiter(Dir);
  if Wildcard = '' then
    FileSpec := FileSpec + '*.*'
  else
    FileSpec := FileSpec + Wildcard;
  Path := IncludeTrailingPathDelimiter(Dir);
  // Do search
  Success := FindFirst(FileSpec, faAnyFile, SR);
  try
    while Success = 0 do
    begin
      // only add true files or directories to list
      if (SR.Name <> '.') and (SR.Name <> '..')
        and (SR.Attr and faVolumeId = 0)
        and (IncludeDirs or not IsDirectory(Path + SR.Name)) then
        List.Add(Path + SR.Name);
      Success := FindNext(SR);
    end;
  finally
    SysUtils.FindClose(SR);
  end;
end;

function LongToShortFilePath(const LongName: string): string;
  {Converts a long file name to the equivalent shortened DOS style 8.3 path.
    @param LongName [in] Long file name to be converted.
    @return Short file name.
  }
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetShortPathName(PChar(LongName), PChar(Result), MAX_PATH));
end;

function FloatToInt(const F: Double): Int64;
  {Converts a floating point number to an integer, rounding to nearest integer.
    @param F [in] Floating point number to be rounded off.
    @return Rounded value as integer.
  }
begin
  // We don't just use Round() on its own because don't want bankers rounding.
  Result := Round(SimpleRoundTo(F, 0));
end;

function DateStamp: string;
  {Creates a date stamp in RFC1123 format
    @return Current date and time as date stamp in UTC/GMT.
  }
const
  // Pattern to create RFC1123 date formats
  cRFC1123Pattern = 'ddd, dd mmm yyyy HH'':''nn'':''ss ''GMT''';
var
  ST: TSystemTime;  // system time
begin
  // This Windows API function gets system time in UTC/GMT
  // see http://msdn.microsoft.com/en-us/library/ms724390
  GetSystemTime(ST);
  // Format system time in RFC1123 format
  Result := FormatDateTime(cRFC1123Pattern, SystemTimeToDateTime(ST));
end;

procedure GetIntf(const Instance: IInterface; const IID: TGUID; out Intf);
  {Get a desired interface pointer to an object instance.
    @param Instance [in] IInterface of instance for which an interface is
      requested. May be nil.
    @param IID [in] Identifier of required interface.
    @param Intf [out] Set to required interface pointer if Instance supports
      interface, or nil if interface not supported or Instance is nil.
  }
begin
  if not Supports(Instance, IID, Intf) then
    Pointer(Intf) := nil;
end;

function IsBaseFileName(const FileName: string): Boolean;
  {Checks if a file name is a base file name (i.e. contains no path
  information).
    @param FileName [in] File name to be tested.
    @return True if file is a base file name, False otherwise.
  }
begin
  Result := (FileName <> '') and (ExtractFileName(FileName) = FileName);
end;

procedure Pause(const ADelay: LongWord);
  {Pauses for a specified number of milliseconds before returning. Performs a
  busy wait.
    @param ADelay [in] Number of milliseconds to pause.
  }

  procedure ProcessMessages;
    {Processes all the messages in program's message queue.
    }
  var
    Msg: TMsg;  // stores message peeked from message loop
  begin
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      if Msg.Message <> WM_QUIT then
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end
      else
        Exit;
    end;
  end;

var
  StartTC: DWORD;   // tick count when routine called
  CurrentTC: Int64; // tick count at each loop iteration
begin
  StartTC := GetTickCount;
  repeat
    ProcessMessages;
    CurrentTC := GetTickCount;
    if CurrentTC < StartTC then
      // tick count has wrapped around: adjust it
      CurrentTC := CurrentTC + High(DWORD);
  until CurrentTC - StartTC >= ADelay;
end;

function IsValidDriveLetter(const C: Char): Boolean;
  {Checks if a character is a valid Windows drive letter.
    @param C [in] Character to be tested.
    @return True if C is a valid drive letter, False otherwise.
  }
begin
  Result := CharInSet(C, ['A'..'Z', 'a'..'z']);
end;

procedure KeyErrorBeep;
  {Emits a sound indicating a keypress error.
  }
begin
  MessageBeep(UINT(-1));
end;

function IsHexDigit(C: Char): Boolean;
  {Checks whether a character is defined as a hex digit.
    @param C [in] Character to be tested.
    @return True if character is a hex digit, False if not.
  }
begin
  Result := CharInSet(C, ['A'..'F', 'a'..'f', '0'..'9']);
end;

function URIBaseName(const URI: string): string;
var
  LastSlashPos: Integer;
begin
//  zzz/zzz.htm
//  12345678901
  LastSlashPos := StrLastPos('/', URI);
  if LastSlashPos = 0 then
    Exit(URI);
  Result := StrSliceRight(URI, Length(URI) - LastSlashPos);
end;

end.

