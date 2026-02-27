{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides information about the open source licenses that are predefined for
 * use with data format meta data.
}


unit DB.MetaData.Licenses;

interface

uses
  // Delphi
  Classes;

type

  TSupportedLicenses = record
  strict private
    type
      TLicenseDataItem = record
        Name: string;
        SPDX: string;
        URL: string;
      end;
    const
      LicenseData: array[1..14] of TLicenseDataItem = (
        (
          Name: 'Apache License 2.0';
          SPDX: 'Apache-2.0';
          URL: 'https://spdx.org/licenses/Apache-2.0.html'
        ),
        (
          Name: 'Boost Software License 1.0';
          SPDX: 'BSL-1.0';
          URL: 'https://spdx.org/licenses/BSL-1.0.html'
        ),
        (
          Name: 'BSD 2-Clause "Simplified" License';
          SPDX: 'BSD-2-Clause';
          URL: 'https://spdx.org/licenses/BSD-2-Clause.html';
        ),
        (
          Name: 'BSD 3-Clause "New" or "Revised" License';
          SPDX: 'BSD-3-Clause';
          URL: 'https://spdx.org/licenses/BSD-3-Clause.html';
        ),
        (
          Name: 'Creative Commons Zero v1.0 Universal License';
          SPDX: 'CC0-1.0';
          URL: 'https://spdx.org/licenses/CC0-1.0.html';
        ),
        (
          Name: 'Eclipse Public License 2.0';
          SPDX: 'EPL-2.0';
          URL: 'https://spdx.org/licenses/EPL-2.0.html';
        ),
        (
          Name: 'GNU Affero General Public License v3.0';
          SPDX: 'AGPL-3.0-only';
          URL: 'https://spdx.org/licenses/AGPL-3.0-only.html';
        ),
        (
          Name: 'GNU General Public License v3.0 only';
          SPDX: 'GPL-3.0-only';
          URL: 'https://spdx.org/licenses/GPL-3.0-only.html';
        ),
        (
          Name: 'GNU General Public License v2.0 only';
          SPDX: 'GPL-2.0-only';
          URL: 'https://spdx.org/licenses/GPL-2.0-only.html';
        ),
        (
          Name: 'GNU Lesser General Public License v3.0 only';
          SPDX: 'LGPL-3.0-only';
          URL: 'https://spdx.org/licenses/LGPL-3.0-only.html';
        ),
        (
          Name: 'GNU Lesser General Public License v2.1 only';
          SPDX: 'LGPL-2.1-only';
          URL: 'https://spdx.org/licenses/LGPL-2.1-only.html';
        ),
        (
          Name: 'MIT License';
          SPDX: 'MIT';
          URL: 'https://spdx.org/licenses/MIT.html';
        ),
        (
          Name: 'Mozilla Public License 2.0';
          SPDX: 'MPL-2.0';
          URL: 'https://spdx.org/licenses/MPL-2.0.html';
        ),
        (
          Name: 'The Unlicense';
          SPDX: 'Unlicense';
          URL: 'https://spdx.org/licenses/Unlicense.html';
        )
      );
    class function IndexOfSPDX(const ASPDX: string): Integer; static;
    class function DataFromSPDX(const ASPDX: string): TLicenseDataItem; static;
  public
    class function ContainsSPDX(const ASPDX: string): Boolean; static;
    class function NameFromSPDX(const ASPDX: string): string; static;
    class function URLFromSPDX(const ASPDX: string): string; static;
    class procedure SPDXList(const AList: TStrings); static;
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  UStrUtils;

{ TSupportedLicenses }

class function TSupportedLicenses.ContainsSPDX(const ASPDX: string): Boolean;
begin
  Result := IndexOfSPDX(ASPDX) >= 0;
end;

class function TSupportedLicenses.DataFromSPDX(const ASPDX: string):
  TLicenseDataItem;
var
  Idx: Integer;
begin
  Idx := IndexOfSPDX(ASPDX);
  if Idx < 0 then
    raise EArgumentException.CreateFmt('Unknown license SPDX: %s', [ASPDX]);
  Result := LicenseData[Idx];
end;

class function TSupportedLicenses.IndexOfSPDX(const ASPDX: string): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := Low(LicenseData) to High(LicenseData) do
  begin
    if StrSameText(ASPDX, LicenseData[Idx].SPDX) then
      Exit(Idx);
  end;
end;

class function TSupportedLicenses.NameFromSPDX(const ASPDX: string): string;
begin
  Result := DataFromSPDX(ASPDX).Name;
end;

class procedure TSupportedLicenses.SPDXList(const AList: TStrings);
var
  Item: TLicenseDataItem;
begin
  for Item in LicenseData do
    AList.Add(Item.SPDX);
end;

class function TSupportedLicenses.URLFromSPDX(const ASPDX: string): string;
begin
  Result := DataFromSPDX(ASPDX).URL;
end;

end.
