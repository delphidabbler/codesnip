{
 * USystemID.pas
 *
 * Exposes a routine that creates a serial number from system properties.
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
 * The Original Code is USystemID.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  // Begin id string Windows product ID
  Result := TOSInfo.ProductID;
  // Append serial number of first fixed hard drive as 8 digit hex string
  for Drive := 0 to 25 do
  begin
    if DriveTypeFromPath(DriveRootPath(Drive)) = DRIVE_FIXED then
    begin
      Serial := HardDiskSerial(DriveRootPath(Drive));
      if Serial <> 0 then
      begin
        Result := Result + IntToHex(Serial, 8);
        Break;
      end;
    end;
  end;
  // Couldn't find a drive serial number: use 0
  Result := Result + IntToHex(0, 8);
end;

end.

