{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Encapsulates a collection's data format metadata.
}


unit DB.MetaData;

{$ScopedEnums ON}

interface

uses
  // Project
  UIStringList,
  UVersionInfo;

type

  ///  <summary>Enumeration of the capabilities of a data format.</summary>
  ///  <remarks>
  ///  <para>- <c>Version</c> - supports the collection's data format version
  ///  number.</para>
  ///  <para>- <c>License</c> - supports collection license information.</para>
  ///  <para>- <c>Copyright</c> - supports collection copyright information.
  ///  </para>
  ///  <para>- <c>Acknowledgements</c> - supports acknowledgements for the
  ///  collection.</para>
  ///  </remarks>
  TMetaDataCap = (
    Version,
    License,
    Copyright,
    Acknowledgements
  );

  ///  <summary>Set of meta data capabilities applying to a data format.
  ///  </summary>
  TMetaDataCaps = set of TMetaDataCap;

  ///  <summary>Record providing information about a collection's license.
  ///  </summary>
  TLicenseInfo = record
  strict private
    var
      fName: string;
      fSPDX: string;
      fURL: string;
      fText: string;
  public
    ///  <summary>Record constructor: sets all fields of record.</summary>
    ///  <remarks>Any or all parameters may be the empty string</remarks>
    constructor Create(const AName, ASPDX, AURL, AText: string);

    ///  <summary>Creates and returns a null record with all fields set to the
    ///  empty string.</summary>
    class function CreateNull: TLicenseInfo; static;

    ///  <summary>Returns a deep copy of this record.</summary>
    function Clone: TLicenseInfo;

    ///  <summary>Name of license.</summary>
    property Name: string read fName;

    ///  <summary>Open Source Initiative SPDX short idenitifier for licenses.
    ///  </summary>
    ///  <remarks>If the license is not supported by the Open Source Initiative
    ///  then this property will be the empty string.</remarks>
    property SPDX: string read fSPDX;

    ///  <summary>URL of license online.</summary>
    ///  <remarks>Optional.</remarks>
    property URL: string read fURL;

    ///  <summary>Full text of license.</summary>
    property Text: string read fText;

    ///  <summary>Returns a string containing license name followed by any URL
    ///  in parentheses.</summary>
    ///  <remarks>If no URL is available then only the license name is returned.
    ///  </remarks>
    function NameWithURL: string;
  end;

  ///  <summary>Record providing informaton about a collection's copyright.
  ///  </summary>
  TCopyrightInfo = record
  strict private
    var
      fDate: string;
      fHolder: string;
      fHolderURL: string;
      fContributors: IStringList;
    function GetContributors: IStringList;
  public
    ///  <summary>Record constructor: sets all fields of record.</summary>
    ///  <remarks>Any or all string parameters may be the empty string.
    ///  <c>AContributors</c> may be <c>nil</c> if there are no contributors.
    ///  </remarks>
    constructor Create(const ADate, AHolder, AHolderURL: string;
      AContributors: IStringList);

    ///  <summary>Creates and returns a null record.</summary>
    ///  <remarks>A null record has all string fields set to the empty string
    ///  and the <c>Contributors</c> property set to an empty list.</c>
    class function CreateNull: TCopyrightInfo; static;

    ///  <summary>Returns a deep copy of this record.</summary>
    function Clone: TCopyrightInfo;

    ///  <summary>Copyright date.</summary>
    ///  <remarks>May be a single year or a range: e.g. 2020 or 2012-2016.
    ///  </remarks>
    property Date: string read fDate;

    ///  <summary>Name of copyright holder.</summary>
    property Holder: string read fHolder;

    ///  <summary>URL of main copyright holder.</summary>
    ///  <remarks>Optional.</remarks>
    property HolderURL: string read fHolderURL;

    ///  <summary>List of names of contributors who share ownership of the
    ///  copyright.</summary>
    property Contributors: IStringList read GetContributors;

    ///  <summary>Creates and returns a string representation of all the
    ///  non-empty fields of the record.</summary>
    function ToString: string;
  end;

  ///  <summary>Encapsulates a collection's meta data.</summary>
  TMetaData = record
  strict private
    var
      fCapabilities: TMetaDataCaps;
      fVersion: TVersionNumber;
      fLicenseInfo: TLicenseInfo;
      fCopyrightInfo: TCopyrightInfo;
      fAcknowledgements: IStringList;
      function GetVersion: TVersionNumber;
      function GetLicenseInfo: TLicenseInfo;
      function GetCopyrightInfo: TCopyrightInfo;
      function GetAcknowledgements: IStringList;
  public
    ///  <summary>Creates a meta data record with specified capabilities.
    ///  </summary>
    ///  <param name="ACapabilities"><c>TMetaDataCaps</c> [in] The required
    ///  capabilities.</param>
    constructor Create(const ACapabilities: TMetaDataCaps);

    ///  <summary>Creates a null meta data record with specified capabilities.
    ///  </summary>
    class function CreateNull: TMetaData; static;

    ///  <summary>Returns a deep copy of this record.</summary>
    function Clone: TMetaData;

   ///  <summary>The meta data capabilities.</summary>
    property Capabilities: TMetaDataCaps
      read fCapabilities;

     ///  <summary>The data format version.</summary>
    property Version: TVersionNumber
      read GetVersion write fVersion;

    ///  <summary>Information about the collection license.</summary>
    property LicenseInfo: TLicenseInfo
      read GetLicenseInfo write fLicenseInfo;

    ///  <summary>Information about the collection's copyright.</summary>
    property CopyrightInfo: TCopyrightInfo
      read GetCopyrightInfo write fCopyrightInfo;

    ///  <summary>List of acknowledgements.</summary>
    property Acknowledgements: IStringList
      read GetAcknowledgements write fAcknowledgements;
  end;

implementation

uses
  // Delphi
  Character,
  // Project
  UStrUtils;

function StandardiseStr(const AStr: string): string;
begin
  Result := StrCompressWhiteSpace(
    StrReplaceChar(
      AStr,
      function(Ch: Char): Boolean
        begin
          Result := TCharacter.IsControl(Ch);
        end,
      ' '
    )
  );
end;

{ TLicenseInfo }

function TLicenseInfo.Clone: TLicenseInfo;
begin
  Result.fName := Name;
  Result.fSPDX := SPDX;
  Result.fURL := URL;
  Result.fText := Text;
end;

constructor TLicenseInfo.Create(const AName, ASPDX, AURL, AText: string);
begin
  fName := StandardiseStr(AName);
  fSPDX := StandardiseStr(ASPDX);
  fURL := StandardiseStr(AURL);
  fText := StandardiseStr(AText);
end;

class function TLicenseInfo.CreateNull: TLicenseInfo;
begin
  Result := TLicenseInfo.Create('', '', '', '');
end;

function TLicenseInfo.NameWithURL: string;
begin
  Result := fName;
  if fURL <> '' then
    Result := Result + ' (' + fURL + ')';
end;

{ TCopyrightInfo }

function TCopyrightInfo.Clone: TCopyrightInfo;
begin
  Result.fDate := Date;
  Result.fHolder := Holder;
  Result.fHolderURL := HolderURL;
  Result.fContributors := Contributors;
end;

constructor TCopyrightInfo.Create(const ADate, AHolder, AHolderURL: string;
  AContributors: IStringList);
begin
  fDate := StandardiseStr(ADate);
  fHolder := StandardiseStr(AHolder);
  fHolderURL := StandardiseStr(AHolderURL);
  fContributors := TIStringList.Create(AContributors);
end;

class function TCopyrightInfo.CreateNull: TCopyrightInfo;
begin
  Result := TCopyrightInfo.Create('', '', '', nil);
end;

function TCopyrightInfo.GetContributors: IStringList;
begin
  Result := TIStringList.Create(fContributors);
end;

function TCopyrightInfo.ToString: string;
resourcestring
  sCopyright = 'Copyright';
begin
  Result := '';
  if Date <> '' then
    Result := Result + '(C) ' + Date;
  if Holder <> '' then
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Holder;
  end;
  if HolderURL <> '' then
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + HolderURL;
  end;
  if Result <> '' then
    Result := sCopyright + ' ' + Result;
end;

{ TMetaData }

function TMetaData.Clone: TMetaData;
begin
  Result.fCapabilities := Capabilities;
  Result.fVersion := GetVersion;
  Result.fLicenseInfo := GetLicenseInfo;
  Result.fCopyrightInfo := GetCopyrightInfo;
  Result.fAcknowledgements := GetAcknowledgements;
end;

constructor TMetaData.Create(const ACapabilities: TMetaDataCaps);
begin
  fCapabilities := ACapabilities;
  fVersion := TVersionNumber.Nul;
  fLicenseInfo := TLicenseInfo.CreateNull;
  fCopyrightInfo := TCopyrightInfo.CreateNull;
  fAcknowledgements := TIStringList.Create;
end;

class function TMetaData.CreateNull: TMetaData;
begin
  Result := TMetaData.Create([]);
end;

function TMetaData.GetAcknowledgements: IStringList;
begin
  if TMetaDataCap.Acknowledgements in fCapabilities then
    Result := TIStringList.Create(fAcknowledgements)
  else
    Result := TIStringList.Create;
end;

function TMetaData.GetCopyrightInfo: TCopyrightInfo;
begin
  if TMetaDataCap.Copyright in fCapabilities then
    Result := fCopyrightInfo.Clone
  else
    Result := TCopyrightInfo.CreateNull;
end;

function TMetaData.GetLicenseInfo: TLicenseInfo;
begin
  if TMetaDataCap.License in fCapabilities then
    Result := fLicenseInfo.Clone
  else
    Result := TLicenseInfo.CreateNull;
end;

function TMetaData.GetVersion: TVersionNumber;
begin
  if TMetaDataCap.Version in fCapabilities then
    Result := fVersion
  else
    Result := TVersionNumber.Nul;
end;

end.
