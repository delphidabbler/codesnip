{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2026, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defined an advanced record that provides a public method to detect the data
 * format, if any, of a vault stored in a directory.
}


unit DB.IO.DataFormatSniffer;

interface

uses
  Classes,
  Generics.Collections,
  DB.DataFormats,
  UIStringList;

type
  TDataFormatSniffer = record
  strict private
    class function IsValidCS4Format(const ADir: string): Boolean; static;
    class function IsValidDCSCV2Format(const ADir: string): Boolean; static;
    class function IsValidNativeVaultFormat(const ADir: string): Boolean;
      static;
  public
    class function DataFormatFromDir(const ADir: string): TDataFormatKind;
      static;
    class function DirContainsVault(const ADir: string): Boolean; static;
    class function PathContainsVault(const ADir: string): Boolean; static;
  end;

implementation

uses
  SysUtils,
  Character,
  IOUtils,

  DB.IO.Vault,
  DB.IO.Vault.Native,
  DB.IO.Vault.DCSCv2,
  DB.IO.Vault.CS4,
  UIOUtils,
  UStrUtils,
  UUtils,
  UVersionInfo;

{ TDataFormatSniffer }

class function TDataFormatSniffer.DataFormatFromDir(const ADir: string):
  TDataFormatKind;
begin
  if not TDirectory.Exists(ADir) then
    Exit(TDataFormatKind.Error);
  if IsValidNativeVaultFormat(ADir) then
    Result := TDataFormatKind.Native_Vault
  else if IsValidCS4Format(ADir) then
    Result := TDataFormatKind.Native_v4
  else if IsValidDCSCV2Format(ADir) then
    Result := TDataFormatKind.DCSC_v2
  else
    Result := TDataFormatKind.Error;
end;

class function TDataFormatSniffer.DirContainsVault(const ADir: string): Boolean;
begin
  Result := DataFormatFromDir(ADir) <> TDataFormatKind.Error;
end;

class function TDataFormatSniffer.IsValidCS4Format(const ADir: string): Boolean;
var
  Validator: IVaultStorageValidator;
begin
  Validator := TCS4VaultStorageValidator.Create(ADir);
  Result := Validator.IsValidStorage;
end;

class function TDataFormatSniffer.IsValidDCSCV2Format(const ADir: string):
  Boolean;
var
  Validator: IVaultStorageValidator;
begin
  Validator := TDCSCV2VaultStorageValidator.Create(ADir);
  Result := Validator.IsValidStorage;
end;

class function TDataFormatSniffer.IsValidNativeVaultFormat(const ADir: string):
  Boolean;
var
  Validator: IVaultStorageValidator;
begin
  Validator := TNativeVaultStorageValidator.Create(ADir);
  Result := Validator.IsValidStorage;
end;

class function TDataFormatSniffer.PathContainsVault(const ADir: string):
  Boolean;
var
  Path: string;
  Root: string;
begin
  // Normalise the path
  Path := ExcludeTrailingPathDelimiter(TPath.GetFullPath(ADir));
  Root := TPath.GetPathRoot(Path);
//  Path := TDirectory.GetParent(Path);
  while not IsSameDirectory(Root, Path) do
  begin
    if DirContainsVault(Path) then
//    if DataFormatFromDir(Path) <> TDataFormatKind.Error then
//    if GetDataFormatFromPath(Path) <> TDataFormatKind.Error then
      Exit(True);
    Path := TDirectory.GetParent(Path);
  end;
  Result := False;
end;

end.

