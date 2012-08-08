{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Ini file access helper routines for use with first-run startup code.
}


unit FirstRun.UIniFile;

interface

function GetIniString(const Section, Key: string; Default: string;
  const FileName: String): string;

function GetIniInt(const Section, Key: string;
  const Default, Min, Max: Longint; const FileName: string): Longint;

procedure SetIniString(const Section, Key, Value, FileName: string);

procedure SetIniInt(const Section, Key: string; const Value: Longint;
  const FileName: string);

procedure DeleteIniSection(const Section, FileName: string);

// Delete key from given section of given ini file.
procedure DeleteIniKey(const Section, Key, FileName: string);

// Checks if a key in given section of an ini file exists
function IniKeyExists(const Section, Key, FileName: string): Boolean;

implementation

uses
  // Delphi
  IniFiles;

function GetIniString(const Section, Key: string; Default: string;
  const FileName: String): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadString(Section, Key, Default);
  finally
    Ini.Free;
  end;
end;

function GetIniInt(const Section, Key: string;
  const Default, Min, Max: Longint; const FileName: string): Longint;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadInteger(Section, Key, Default);
  finally
    Ini.Free;
  end;
end;

procedure SetIniString(const Section, Key, Value, FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString(Section, Key, Value);
  finally
    Ini.Free;
  end;
end;

procedure SetIniInt(const Section, Key: string; const Value: Longint;
  const FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger(Section, Key, Value);
  finally
    Ini.Free;
  end;
end;

procedure DeleteIniSection(const Section, FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.EraseSection(Section);
  finally
    Ini.Free;
  end;
end;

// Delete key from given section of given ini file.
procedure DeleteIniKey(const Section, Key, FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.DeleteKey(Section, Key);
  finally
    Ini.Free;
  end;
end;

// Checks if a key in given section of an ini file exists
function IniKeyExists(const Section, Key, FileName: string): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ValueExists(Section, Key);
  finally
    Ini.Free;
  end;
end;

end.
