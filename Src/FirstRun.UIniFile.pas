{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Ini file access helper routines for use with first-run startup code.
}


unit FirstRun.UIniFile;


interface


///  <summary>Gets a string value from an ini file.</summary>
///  <param name="Section">string [in] Ini file section containing key.</param>
///  <param name="Key">string [in] Name of key whose value is required.</param>
///  <param name="Default">string [in] Default value to be returned if key can't
///  be read.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
///  <returns>string. Required key value or Default if key can't be read.
///  </returns>
function GetIniString(const Section, Key: string; Default: string;
  const FileName: String): string;

///  <summary>Gets an integer value from an ini file.</summary>
///  <param name="Section">string [in] Ini file section containing key.</param>
///  <param name="Key">string [in] Name of key whose value is required.</param>
///  <param name="Default">LongInt [in] Default value to be returned if key
///  can't be read.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
///  <returns>LongInt. Required key value or Default if key can't be read.
///  </returns>
function GetIniInt(const Section, Key: string; const Default: Longint;
  const FileName: string): Longint;

///  <summary>Writes a string value to an ini file.</summary>
///  <param name="Section">string [in] Ini file section containing key.</param>
///  <param name="Key">string [in] Name of key that receives value.</param>
///  <param name="Value">string [in] Value to be stored.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
procedure SetIniString(const Section, Key, Value, FileName: string);

///  <summary>Writes an integer value to an ini file.</summary>
///  <param name="Section">string [in] Ini file section containing key.</param>
///  <param name="Key">string [in] Name of key that receives value.</param>
///  <param name="Value">LongInt [in] Value to be stored.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
procedure SetIniInt(const Section, Key: string; const Value: Longint;
  const FileName: string);

///  <summary>Deletes a section of an ini file.</summary>
///  <param name="Section">string [in] Section to be deleted.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
procedure DeleteIniSection(const Section, FileName: string);

///  <summary>Delete a key / value pair from an ini file.</summary>
///  <param name="Section">string [in] Ini file section containing key.</param>
///  <param name="Key">string [in] Name of key to be deleted.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
procedure DeleteIniKey(const Section, Key, FileName: string);

///  <summary>Checks if a section exists in an ini file.</summary>
///  <param name="Section">string [in] Ini file section to be deleted.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
///  <returns>Boolean. True if section exists, False if not.</returns>
function IniSectionExists(const Section, FileName: string): Boolean;

///  <summary>Checks if a key exists in an ini file.</summary>
///  <param name="Section">string [in] Ini file section containing key.</param>
///  <param name="Key">string [in] Name of required key.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
///  <returns>Boolean. True if key exists, False if not.</returns>
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

function GetIniInt(const Section, Key: string; const Default: Longint;
  const FileName: string): Longint;
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

function IniSectionExists(const Section, FileName: string): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.SectionExists(Section);
  finally
    Ini.Free;
  end;
end;

end.

