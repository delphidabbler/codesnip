{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Exposes a routine that creates a serial number from system properties.
}


unit USystemID;


interface


function SystemIDStr: string;
  {Builds an ID string from the Windows product id and the serial number of the
  first hard disk drive on the system.
    @return Required ID string.
  }


implementation


uses
  // Delphi
  SysUtils, Windows,
  // Project
  {$IFDEF PORTABLE}
  UAppInfo,
  {$ENDIF}
  USystemInfo;


function HardDiskSerial(const Drive: string): DWORD;
  {Gets the serial number of a disk disk.
    @param Drive [in] Name of drive (e.g. C:\) or network share (e.g.
      \\MyServer\MyShare\). A trailing backslash must be provided.
    @return Serial number or 0 if serial number can't be found or if drive has
      no serial number.
  }
var
  Unused: DWORD;        // unused parameters
  PrevErrorMode: UINT;  // stores Windows error mode
begin
  // Inhibit system dialog appearing on error
  PrevErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := 0;
    GetVolumeInformation(
      PChar(Drive), nil, 0, @Result, Unused, Unused, nil, 0
    );
  finally
    // Restore old error mode
    SetErrorMode(PrevErrorMode);
  end;
end;

function DriveTypeFromPath(const Path: string): Integer;
  {Determines type of a drive.
    @param Path [in] Path containing drive name (e.g. C:\).
    @return Code representing drive type.
  }
var
  Drive: string;  // the drive name
begin
  Drive := ExtractFileDrive(Path) + '\';
  Result := Integer(GetDriveType(PChar(Drive)));
end;

function IsValidDriveNum(const DriveNum: Byte): Boolean;
  {Checks if a zero-based drive number exists on the system.
    @param [in] Drive number in range 0..25.
    @return True if drive is valid, false otherwise.
  }
begin
  if DriveNum in [0..25] then
    Result := GetLogicalDrives and (1 shl DriveNum) <> 0
  else
    Result := False;
end;

function DriveRootPath(const DriveNum: Byte): string;
  {Gets root drive path for a zero-based drive number.
    @param DriveNum [in] Drive number in range 0..25.
    @return Drive root (e.g. C:\).
  }
begin
  if DriveNum in [0..25] then
    Result := Char(DriveNum + Ord('A')) + ':\'
  else
    Result := '';
end;

function SystemIDStr: string;
  {Builds an ID string from the Windows product id and the serial number of the
  first hard disk drive on the system.
    @return Required ID string.
  }
var
  Drive: Byte;    // loops through all drive numbers
  Serial: DWORD;  // serial number of a hard diak
begin
  {$IFDEF PORTABLE}
  // Get serial for removable disk if possible. Fall thru and get hard disk if
  // not.
  Serial := HardDiskSerial(
    IncludeTrailingPathDelimiter(ExtractFileDrive(TAppInfo.AppExeDir))
  );
  if Serial <> 0 then
    Exit(TOSInfo.ProductID + IntToHex(Serial, 8));
  {$ENDIF}
  // Append serial number of first fixed hard drive as 8 digit hex string
  for Drive := 0 to 25 do
  begin
    if DriveTypeFromPath(DriveRootPath(Drive)) = DRIVE_FIXED then
    begin
      Serial := HardDiskSerial(DriveRootPath(Drive));
      if Serial <> 0 then
        Exit(TOSInfo.ProductID + IntToHex(Serial, 8));
    end;
  end;
  // Couldn't find a drive serial number: use 0
  Result := TOSInfo.ProductID + IntToHex(0, 8);
end;

end.

