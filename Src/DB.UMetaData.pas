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
  // Delphi
  Generics.Collections,
  // Project
  DB.DataFormats,
  UIStringList,
  UVersionInfo;


type

  TMetaDataCapability = (
    mdcVersion,
    mdcLicense,
    mdcCopyright,
    mdcContributors,
    mdcTesters      {TODO -cView: rename as mdcAcknowledgements}
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

    ///  <summary>Creates and returns a null record with all fields set to the
    ///  empty string.</summary>
    class function CreateNull: TDBLicenseInfo; static;

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
    ///  <summary>Creates and returns a null record with all fields set to the
    ///  empty string.</summary>
    class function CreateNull: TDBCopyrightInfo; static;
    ///  <summary>Copyright date.</summary>
    ///  <remarks>May be a single year or a range: e.g. 2020 or 2012-2016.
    ///  </remarks>
    property Date: string read fDate;
    ///  <summary>Name of copyright holder.</summary>
    property Holder: string read fHolder;
    ///  <summary>URL of main copyright holder.</summary>
    ///  <remarks>Optional.</remarks>
    property HolderURL: string read fHolderURL;
    ///  <summary>Creates and returns a string representation of all the
    ///  non-empty fields of the record.</summary>
    function ToString: string;
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

  ///  <summary>Base class for all classes that implement <c>IDBMetaData</c>
  ///  and can also be registered with <c>TMetaDataFactory</c>.</summary>
  TRegisterableMetaData = class abstract(TInterfacedObject)
  public
    ///  <summary>Creates an instance of a concrete descendant of this class.
    ///  </summary>
    ///  <param name="ACollection"><c>TCollection</c> [in] Collection associated
    ///  with the meta data being created.</param>
    ///  <returns><c>IDBMetaData</c>. Required meta data object.</returns>
    class function Instance(const AStorageDetails: TDataStorageDetails):
      IDBMetaData;
      virtual; abstract;
    ///  <summary>Gets the meta data capabilities for the collection data
    ///  format.</summary>
    ///  <returns><c>TMetaDataCapabilities</c>. Required meta data capabilities.
    ///  </returns>
    ///  <remarks>This method enables meta data capabilities to be obtained
    ///  without creating an instance of the object.</remarks>
    class function Capabilities: TMetaDataCapabilities; virtual; abstract;
    ///  <summary>Returns information about what, if any, meta data is supported
    ///  by a collection.</summary>
    ///  <remarks>This method provides a means of accessing the data returned by
    ///  the <c>Capabilities</c> class method from <c>IDBMetaData</c> instances.
    ///  </remarks>
    function GetCapabilities: TMetaDataCapabilities; virtual;
  end;

  TRegisterableMetaDataClass = class of TRegisterableMetaData;

  ///  <summary>Advanced record manages the registration and creation of meta
  ///  data objects for different collection data formats.</summary>
  TMetaDataFactory = record
  strict private
    class var
      {TODO -Refactor: rename fCallbackMap since it does not contain callback,
            but does contain class types.}
      ///  <summary>Map of collection format kinds to classes that implement the
      ///  format's meta data.</summary>
      fCallbackMap: TDictionary<TDataFormatKind, TRegisterableMetaDataClass>;
  public
    class constructor Create;
    class destructor Destroy;
    ///  <summary>Registers a class type that can create a meta data object for
    ///  a given collection data format kind.</summary>
    ///  <param name="AFormat"><c>TCollectionFormatKind</c> [in] Collection data
    ///  format for which the meta data class is being registered.</param>
    ///  <param name="AClass"><c>TRegisterableMetaDataClass</c> [in] Type of
    ///  class to create.</param>
    class procedure RegisterCreator(AFormat: TDataFormatKind;
      AClass: TRegisterableMetaDataClass); static;
    ///  <summary>Creates a meta data object instance that can read a given
    ///  collection data format.</summary>
    ///  <param name="ACollection"><c>TCollection</c> [in] Collection for which
    ///  meta data reader object is required.</param>
    ///  <returns><c>IDBMetaData</c>. Requested object. May be a null object if
    ///  no meta data class was registered for the data format associated with
    ///  <c>ACollection</c>.</returns>
    class function CreateInstance(const AStorageDetails: TDataStorageDetails):
      IDBMetaData; static;
    ///  <summary>Gets the meta data capabilities for a collection data format.
    ///  </summary>
    ///  <param name="AFormat"><c>TCollectionFormatKind</c> [in] Collection data
    ///  format for which meta data capabilities are required.</param>
    ///  <returns><c>TMetaDataCapabilities</c>. Required meta data capabilities.
    ///  </returns>
    class function CapabilitiesOf(const AFormat: TDataFormatKind):
      TMetaDataCapabilities; static;
  end;

implementation

uses
  // Delphi
  SysUtils,
  Character,
  // Project
  UConsts,
  UStrUtils;

type
  ///  <summary>Implements a null, do nothing, meta data object.</summary>
  ///  <remarks>Instance of this class are used when a collection format does
  ///  not support meta data.</remarks>
  TNullMetaData = class(TInterfacedObject, IDBMetaData)
  public
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

{ TDBLicenseInfo }

constructor TDBLicenseInfo.Create(const AName, ASPDX, AURL, AText: string);

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

begin
  fName := StandardiseStr(AName);
  fSPDX := StandardiseStr(ASPDX);
  fURL := StandardiseStr(AURL);
  fText := StandardiseStr(AText);
end;

class function TDBLicenseInfo.CreateNull: TDBLicenseInfo;
begin
  Result := TDBLicenseInfo.Create('', '', '', '');
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

class function TDBCopyrightInfo.CreateNull: TDBCopyrightInfo;
begin
  Result := TDBCopyrightInfo.Create('', '', '');
end;

function TDBCopyrightInfo.ToString: string;
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

{ TRegisterableMetaData }

function TRegisterableMetaData.GetCapabilities: TMetaDataCapabilities;
begin
  Result := Capabilities;
end;

{ TMetaDataFactory }

class function TMetaDataFactory.CapabilitiesOf(
  const AFormat: TDataFormatKind): TMetaDataCapabilities;
begin
  if fCallbackMap.ContainsKey(AFormat) then
    Result := fCallbackMap[AFormat].Capabilities
  else
    Result := [];
end;

class constructor TMetaDataFactory.Create;
begin
  fCallbackMap := TDictionary<
    TDataFormatKind, TRegisterableMetaDataClass
  >.Create;
end;

class function TMetaDataFactory.CreateInstance(
  const AStorageDetails: TDataStorageDetails): IDBMetaData;
begin
  if fCallbackMap.ContainsKey(AStorageDetails.Format) then
    Result := fCallbackMap[AStorageDetails.Format].Instance(AStorageDetails)
  else
    Result := TNullMetaData.Create;
end;

class destructor TMetaDataFactory.Destroy;
begin
  fCallBackMap.Free;
end;

class procedure TMetaDataFactory.RegisterCreator(
  AFormat: TDataFormatKind; AClass: TRegisterableMetaDataClass);
begin
  fCallbackMap.AddOrSetValue(AFormat, AClass);
end;

{ TNullMetaData }

function TNullMetaData.GetCapabilities: TMetaDataCapabilities;
begin
  Result := [];
end;

function TNullMetaData.GetContributors: IStringList;
begin
  Result := TIStringList.Create;
end;

function TNullMetaData.GetCopyrightInfo: TDBCopyrightInfo;
begin
  Result := TDBCopyrightInfo.CreateNull;
end;

function TNullMetaData.GetLicenseInfo: TDBLicenseInfo;
begin
  Result := TDBLicenseInfo.CreateNull;
end;

function TNullMetaData.GetTesters: IStringList;
begin
  Result := TIStringList.Create;
end;

function TNullMetaData.GetVersion: TVersionNumber;
begin
  Result := TVersionNumber.Nul;
end;

function TNullMetaData.IsCorrupt: Boolean;
resourcestring
  sNotSupportedError = 'Can''t call IDBMetaData.IsCorrupt for null meta data';
begin
  raise ENotSupportedException.Create(sNotSupportedError);
end;

function TNullMetaData.IsRecognised: Boolean;
begin
  Result := False;
end;

function TNullMetaData.IsSupportedVersion: Boolean;
begin
  Result := False;
end;

procedure TNullMetaData.Refresh;
begin
  // Do nothing
end;

end.
