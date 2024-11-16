{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Declares interface and defines records required to support collection
 * metadata.
}


unit DB.UMetaData;


interface


uses
  // Project
  UIStringList,
  UVersionInfo;


type

  TMetaDataCapability = (
    mdcVersion,
    mdcLicense,
    mdcCopyright,
    mdcContributors,
    mdcTesters
  );

  TMetaDataCapabilities = set of TMetaDataCapability;

  ///  <summary>Record providing information about a collection's license.
  ///  </summary>
  TDBLicenseInfo = record
  strict private
    fName: string;
    fSPDX: string;
    fURL: string;
    fText: string;
  public
    ///  <summary>Record constructor: sets all fields of record.</summary>
    ///  <remarks>Any or all parameters may be the empty string</remarks>
    constructor Create(const AName, ASPDX, AURL, AText: string);
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
  TDBCopyrightInfo = record
  strict private
    fDate: string;
    fHolder: string;
    fHolderURL: string;
  public
    ///  <summary>Record constructor: sets all fields of record.</summary>
    ///  <remarks>Any or all parameters may be the empty string</remarks>
    constructor Create(const ADate, AHolder, AHolderURL: string);
    ///  <summary>Copyright date.</summary>
    ///  <remarks>May be a single year or a range: e.g. 2020 or 2012-2016.
    ///  </remarks>
    property Date: string read fDate;
    ///  <summary>Name of copyright holder.</summary>
    property Holder: string read fHolder;
    ///  <summary>URL of main copyright holder.</summary>
    ///  <remarks>Optional.</remarks>
    property HolderURL: string read fHolderURL;
  end;

  ///  <summary>Interface that provides information about any meta data
  ///  supported by a collection.</summary>
  IDBMetaData = interface(IInterface)
    ///  <summary>Returns information about what, if any, meta data is supported
    ///  by a collection.</summary>
    function GetCapabilities: TMetaDataCapabilities;
    ///  <summary>Returns the collection's version number.</summary>
    ///  <remarks>A null version number is returned if the collection does not
    ///  include <c>Version</c> in its capabilities.</remarks>
    function GetVersion: TVersionNumber;
    ///  <summary>Returns the collection's license information.</summary>
    ///  <remarks>Return value is meaningless if the collection does not include
    ///  <c>License</c> in its capabilities.</remarks>
    function GetLicenseInfo: TDBLicenseInfo;
    ///  <summary>Returns the collection's copyright informatiom.</summary>
    ///  <remarks>Return value is meaningless if the collection does not include
    ///  <c>Copyright</c> in its capabilities.</remarks>
    function GetCopyrightInfo: TDBCopyrightInfo;
    ///  <summary>Returns a list of contributors to the collection.</summary>
    ///  <remarks>Return value is meaningless if the collection does not include
    ///  <c>Contributors</c> in its capabilities.</remarks>
    function GetContributors: IStringList;
    ///  <summary>Returns a list of testers of the collection.</summary>
    ///  <remarks>Return value is meaningless if the collection does not include
    ///  <c>Testers</c> in its capabilities.</remarks>
    function GetTesters: IStringList;
    ///  <summary>Checks if meta data is recognised as belonging to a valid
    ///  collection, whether supported or not.</summary>
    function IsRecognised: Boolean;
    ///  <summary>Checks if meta data is recognised as belonging to a supported
    ///  collection version.</summary>
    function IsSupportedVersion: Boolean;
    ///  <summary>Checks if meta data is corrupt.</summary>
    ///  <summary>Should only be called if meta data belongs to a supported
    ///  collection. An exception should be raised if called on unsupported
    ///  versions.</summary>
    function IsCorrupt: Boolean;
    ///  <summary>Refreshes the meta information by re-reading from collection
    ///  meta files.</summary>
    procedure Refresh;
  end;


implementation


{ TDBLicenseInfo }

constructor TDBLicenseInfo.Create(const AName, ASPDX, AURL, AText: string);
begin
  fName := AName;
  fSPDX := ASPDX;
  fURL := AURL;
  fText := AText;
end;

function TDBLicenseInfo.NameWithURL: string;
begin
  Result := fName;
  if fURL <> '' then
    Result := Result + ' (' + fURL + ')';
end;

{ TDBCopyrightInfo }

constructor TDBCopyrightInfo.Create(const ADate, AHolder, AHolderURL: string);
begin
  fDate := ADate;
  fHolder := AHolder;
  fHolderURL := AHolderURL;
end;

end.
