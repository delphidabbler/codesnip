{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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

///  <summary>Renames a section of an ini file, preserving all values.</summary>
///  <param name="OldSection">string [in] Name of section to be renamed.</param>
///  <param name="NewSection">string [in] New name of section.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
procedure RenameIniSection(const OldSection, NewSection, FileName: string);

///  <summary>Renames a key in a given section of an ini file if the key exists.
///  </summary>
///  <param name="Section">string [in] Ini file section containing key.</param>
///  <param name="OldKey">string [in] Name of key to be renamed.</param>
///  <param name="NewKey">string [in] New name of key.</param>
///  <param name="FileName">string [in] Name of ini file.</param>
///  <returns>Boolean. True if key exists and was renamed, False if key does not
///  exist and no action was taken.</returns>
function RenameIniKey(const Section, OldKey, NewKey, FileName: string):
  Boolean;


implementation


uses
  // Delphi
  IniFiles, Classes;


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

procedure RenameIniSection(const OldSection, NewSection, FileName: string);
var
  Ini: TIniFile;
  AllValues: TStringList;
  I: Integer;
begin
  AllValues := nil;
  Ini := TIniFile.Create(FileName);
  try
    if not Ini.SectionExists(OldSection) then
      Exit;
    AllValues := TStringList.Create;
    Ini.ReadSectionValues(OldSection, AllValues);
    for I := 0 to Pred(AllValues.Count) do
      Ini.WriteString(
        NewSection, AllValues.Names[I], AllValues.ValueFromIndex[I]
      );
    Ini.EraseSection(OldSection);
  finally
    AllValues.Free;
    Ini.Free;
  end;
end;

function RenameIniKey(const Section, OldKey, NewKey, FileName: string):
  Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ValueExists(Section, OldKey);
    if not Result then
      Exit;
    Ini.WriteString(Section, NewKey, Ini.ReadString(Section, OldKey, ''));
    Ini.DeleteKey(Section, OldKey);
  finally
    Ini.Free;
  end;
end;

end.

