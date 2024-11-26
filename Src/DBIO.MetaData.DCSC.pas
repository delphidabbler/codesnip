{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Loads and vaildates meta data supported by the DelphiDabbler Code Snippets
 * Collection.
}


unit DBIO.MetaData.DCSC;

interface

{TODO -cVault: Remove support for main database v1 (and perhaps v>2)}

{
  Notes About Database Versions And Meta Files.
  =============================================

  Versions
  --------

  v1 of the Code Snippets Database was not considered to have a version number
  until the arrival of v2. Therefore v1 did not have any means of identifying
  its version number. Although the database format changed slightly over time
  there is not enough historical information to identify different minor
  releases, so all are considered to be v1.0.0.0

  Database v2 has a VERSION file that specifies the version number as a dotted
  quad, of the form v2.0.0.0.

  Note that semantic versioning is now being used so any database with major
  version 2 will be backwards compatible with earlier v2 minor release.

  If a breaking change is introduced the major version will be bumped to v3 and
  so on.

  Meta Files
  ----------

  Database v1 had only two meta files:
    + contrib.txt - lists of contributors to the database, one per line
    + testers.txt - lists of database testers, one per line
  If those two files are present the database is assumed to be v1

  Database v2 has the following meta files:
    + VERSION       - version number (v2.x.x.x)
    + CONTRIBUTORS  - lists of contributors to the database, one per line
    + TESTERS       - lists of database testers, one per line
    + LICENSE       - full text of the license that applies to the snippets
    + LICENSE-INFO  - furter information about the license

  For database v1 the license text, license information are hard-wired in the
  absence of meta files. As noted above the version number is deduced.

  File encoding
  -------------

  All meta files are plain text.

  Early v1 database meta files were encoded in the system default encoding.
  Later v1 databases encoded meta files in UTF-8. To distinguish between UTF-8
  and default encoded files the UTF-8 files use the UTF-8 preamble (BOM).

  v2 database meta files are all encoded in UTF-8 with preamble (BOM).

  Future major versions
  ---------------------

  Future major database releases MUST include backwards compatibility with
  earlier versions of CodeSnip in as much as those versions must be able to
  easily detect and reject the format.

  To achieve this new major releases MUST include a VERSION file encoded in
  UTF-8 with BOM. Existing code can detect and will reject any unsupported
  version.

  CodeSnip support
  ----------------

  CodeSnip versions earlier than v4.16 are only ever delivered v1 database files
  via their built in web update code. There is no way for these versions to
  access v2 databases.

  CodeSnip from v4.16 get their main snippets database from files downloaded
  manually. All such databases are v2. CodeSnip v4.16 also supports v1 format
  because such a database may be resident on the user's computer at install
  time.
}



uses
  // Project
  SysUtils,
  Types,
  // VCL
  DB.UCollections,
  DB.UMetaData,
  UIStringList,
  UStructs,
  UVersionInfo;


const
  DBValidVersions: TRange = (Min: 1; Max: 2);

type

  ///  <summary>Provides names of meta data files supported by various database
  ///  versions.</summary>
  TDBMetaFileNames = record
  public
    const
      ContributorsV1    = 'contrib.txt';
      TestersV1         = 'testers.txt';
      VersionV2AndLater = 'VERSION';
      LicenseV2         = 'LICENSE';
      LicenseInfoV2     = 'LICENSE-INFO';
      ContributorsV2    = 'CONTRIBUTORS';
      TestersV2         = 'TESTERS';
  end;

  ///  <summary>Abstract base class for classes that access or emulate database
  ///  meta data files.</summary>
  TDBMetaFiles = class abstract(TObject)
  strict private
    var
      ///  <summary>Directory of database for which meta data files are being
      ///  accessed.</summary>
      fDBDir: string;
    ///  <summary>Returns encoding used by given meta file.</summary>
    function GetFileEncoding(const FileName: TFileName): TEncoding;
  strict protected
    ///  <summary>Makes a fully specified path to a given database file.
    ///  </summary>
    ///  <remarks>FileName must contain no path information.</remarks>
    function MakePath(const FileName: string): string;
    ///  <summary>Checks if a given file exists in database directory.</summary>
    function DBFileExists(const FileName: string): Boolean;
    ///  <summary>Reads all lines from given file and returns them as an array.
    ///  </summary>
    ///  <remarks>FileName must contain no path information.</remarks>
    function ReadFileLines(const FileName: TFileName): TStringDynArray;
    ///  <summary>Reads all text from given file and returns it.</summary>
    ///  <remarks>FileName must contain no path information.</remarks>
    function ReadFileText(const FileName: TFileName): string;
  public
    ///  <summary>Constructs object that accesses meta data database files in
    ///  given directory.</summary>
    constructor Create(const DBDir: string);
    ///  <summary>Returns text of the version file, or surrogate value.
    ///  </summary>
    function Version: string; virtual; abstract;
    ///  <summary>Returns content license text file or surrogate value.
    ///  </summary>
    function LicenseText: string; virtual; abstract;
    ///  <summary>Returns lines of license information file or surrogate value.
    ///  </summary>
    function LicenseInfo: TStringDynArray; virtual; abstract;
    ///  <summary>Returns lines of contributors file or surrogate value.
    ///  </summary>
    function Contributors: TStringDynArray; virtual; abstract;
    ///  <summary>Returns lines of testers file or surrogate value.</summary>
    function Testers: TStringDynArray; virtual; abstract;
    ///  <summary>Checks if all the expected meta files are present. Returns
    ///  True if so or False if not.</summary>
    ///  <remarks>ENotSupportedException must be raised if called on an
    ///  unsupported database version.</remarks>
    function AreAllFilesPresent: Boolean; virtual; abstract;
  end;

  ///  <summary>Class that accesses content of version 1 main database meta data
  ///  files.</summary>
  ///  <remarks>Not all meta files are present in version main databases so
  ///  invariant placeholder content is provided to substitute for missing
  ///  files.</remarks>
  TV1DBMetaFiles = class sealed(TDBMetaFiles)
  strict private
    const
      cContributorsFile = TDBMetaFileNames.ContributorsV1;
      cTestersFile = TDBMetaFileNames.TestersV1;
  public
    ///  <summary>Returns an surrogate, invariant value of 1 for the version
    ///  number.</summary>
    ///  <remarks>No version file exists for this database version. The value
    ///  returned is deduced using documentation from the current code snippets
    ///  database project.</remarks>
    function Version: string; override;
    ///  <summary>Returns an surrogate, invariant value for the license text.
    ///  </summary>
    ///  <remarks>No license text file exists for this database version. The
    ///  value returned is based on documentation of the database.</remarks>
    function LicenseText: string; override;
    ///  <summary>Returns an surrogate, invariant value for the lines of license
    ///  information.</summary>
    function LicenseInfo: TStringDynArray; override;
    ///  <summary>Returns the lines of the contributors file.</summary>
    function Contributors: TStringDynArray; override;
    ///  <summary>Returns the lines of the testers file.</summary>
    function Testers: TStringDynArray; override;
    ///  <summary>Checks if all the expected meta files are present. Returns
    ///  True if so or False if not.</summary>
    function AreAllFilesPresent: Boolean; override;
  end;

  ///  <summary>Class that accesses content of supported version 2 main database
  ///  meta files.</summary>
  TV2DBMetaFiles = class sealed(TDBMetaFiles)
  strict private
    const
      cVersionFile = TDBMetaFileNames.VersionV2AndLater;
      cLicenseFile = TDBMetaFileNames.LicenseV2;
      cLicenseInfoFile = TDBMetaFileNames.LicenseInfoV2;
      cContributorsFile = TDBMetaFileNames.ContributorsV2;
      cTestersFile = TDBMetaFileNames.TestersV2;
  public
    ///  <summary>Returns the contents of the version file.</summary>
    function Version: string; override;
    ///  <summary>Returns the contents of the license text file.</summary>
    function LicenseText: string; override;
    ///  <summary>Returns the lines of the license info file.</summary>
    function LicenseInfo: TStringDynArray; override;
    ///  <summary>Returns the lines of the contributors file.</summary>
    function Contributors: TStringDynArray; override;
    ///  <summary>Returns the lines of the testers file.</summary>
    function Testers: TStringDynArray; override;
    ///  <summary>Checks if all the expected meta files are present. Returns
    ///  True if so or False if not.</summary>
    function AreAllFilesPresent: Boolean; override;
  end;

  ///  <summary>Class that represents later versions of database meta file
  ///  formats.</summary>
  ///  <remarks>These formats have a valid version file but the version is not
  ///  supported and nothing is known about any other meta data files.</remarks>
  TLaterDBMetaFiles = class sealed(TDBMetaFiles)
  strict private
    const
      cVersionFile = TDBMetaFileNames.VersionV2AndLater;
  public
    ///  <summary>Returns the contents of the version file.</summary>
    function Version: string; override;
    ///  <summary>Returns the empty string.</summary>
    ///  <remarks>The file format is unknown, so the license text file cannot be
    ///  read and there is no information to deduce the value.</remarks>
    function LicenseText: string; override;
    ///  <summary>Returns an empty string array.</summary>
    ///  <remarks>The file format is unknown, so the license information file
    ///  cannot be read and there is no information to deduce the value.
    ///  </remarks>
    function LicenseInfo: TStringDynArray; override;
    ///  <summary>Returns an empty string array.</summary>
    ///  <remarks>The file format is unknown, so the contributors file cannot be
    ///  read and there is no information to deduce the value.</remarks>
    function Contributors: TStringDynArray; override;
    ///  <summary>Returns an empty string array.</summary>
    ///  <remarks>The file format is unknown, so the testers file cannot be read
    ///  and there is no information to deduce the value.</remarks>
    function Testers: TStringDynArray; override;
    ///  <summary>Checks if all the expected meta files are present only if
    ///  the meta files come from a supported database format.</summary>
    ///  <exception>ENotSupportedException always raised since there is no way
    ///  of knowing what files should be present in an unsupported database
    ///  format.</exception>
    function AreAllFilesPresent: Boolean; override;
  end;

  ///  <summary>Class that is present to represent unknown database meta file
  ///  formats. Also used when database is not present.</summary>
  ///  <remarks>Accesses no files and returns null results for all methods
  ///  except IsVersionSupported.</remarks>
  TUnknownOrMissingMetaFiles = class sealed(TDBMetaFiles)
  public
    ///  <summary>Returns the empty string.</summary>
    ///  <remarks>The file format is unknown, so the version file cannot be read
    ///  and there is no information to deduce the value.</remarks>
    function Version: string; override;
    ///  <summary>Returns the empty string.</summary>
    ///  <remarks>The file format is unknown, so the license text file cannot be
    ///  read and there is no information to deduce the value.</remarks>
    function LicenseText: string; override;
    ///  <summary>Returns an empty string array.</summary>
    ///  <remarks>The file format is unknown, so the license information file
    ///  cannot be read and there is no information to deduce the value.
    ///  </remarks>
    function LicenseInfo: TStringDynArray; override;
    ///  <summary>Returns an empty string array.</summary>
    ///  <remarks>The file format is unknown, so the contributors file cannot be
    ///  read and there is no information to deduce the value.</remarks>
    function Contributors: TStringDynArray; override;
    ///  <summary>Returns an empty string array.</summary>
    ///  <remarks>The file format is unknown, so the testers file cannot be read
    ///  and there is no information to deduce the value.</remarks>
    function Testers: TStringDynArray; override;
    ///  <summary>Checks if all the expected meta files are present only if
    ///  the meta files come from a supported database format.</summary>
    ///  <exception>ENotSupportedException always raised since there is no way
    ///  of knowing what files should be present in an unrecognised database
    ///  format.</exception>
    function AreAllFilesPresent: Boolean; override;
  end;

  ///  <summary>Factory to create the correct instance of database meta file
  ///  classes that can read the version of a database in a given folder.
  ///  </summary>
  TDBMetaFilesFactory = record
  public
    class function GetInstance(const DBDir: string): TDBMetaFiles;
      static;
  end;

  ///  <summary>Abstract base class for classes that provide main database meta
  ///  data.</summary>
  TAbstractMainDBMetaData = class abstract(TRegisterableMetaData)
  strict private
    var
      fMetaFiles: TDBMetaFiles;
      fIsVersionLoaded: Boolean;
      fVersion: TVersionNumber;
      fIsLicenseAndCopyrightInfoLoaded: Boolean;
      fLicenseInfo: TDBLicenseInfo;
      fCopyrightInfo: TDBCopyrightInfo;
      fContributors: IStringList;
      fTesters: IStringList;

    procedure LoadLicenseAndCopyrightInfo;

  strict protected
    function GetDBDir: string; virtual; abstract;
  public
    ///  <summary>Creates an instance of meta data object that can read this
    ///  collection's format.</summary>
    ///  <remarks>Must be called from a concrete descendant class.</remarks>
    class function Instance(ACollection: TCollection): IDBMetaData; override;
    ///  <summary>Gets the meta data capabilities for the collection data
    ///  format.</summary>
    ///  <returns><c>TMetaDataCapabilities</c>. Required meta data capabilities.
    ///  </returns>
    ///  <remarks>This method enables meta data capabilities to be obtained
    ///  without creating an instance of the object.</remarks>
    class function Capabilities: TMetaDataCapabilities; override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    ///  <summary>Returns database version number.</summary>
    ///  <remarks>
    ///  <para>A null version number is returned if the meta data does not come
    ///  from a recognised database.</para>
    ///  <para>Method of IDBMetaData.</para>
    ///  </remarks>
    function GetVersion: TVersionNumber;
    ///  <summary>Returns database license information.</summary>
    ///  <remarks>
    ///  <para>Return value is meaningless if the meta data does not come
    ///  from a supported database.</para>
    ///  <para>Method of IDBMetaData.</para>
    ///  </remarks>
    function GetLicenseInfo: TDBLicenseInfo;
    ///  <summary>Returns database copyright informatiom.</summary>
    ///  <remarks>
    ///  <para>Return value is meaningless if the meta data does not come
    ///  from a supported database.</para>
    ///  <para>Method of IDBMetaData.</para>
    ///  </remarks>
    function GetCopyrightInfo: TDBCopyrightInfo;
    ///  <summary>Returns list of contributors to database.</summary>
    ///  <remarks>
    ///  <para>Return value is meaningless if the meta data does not come
    ///  from a supported database.</para>
    ///  <para>Method of IDBMetaData.</para>
    ///  </remarks>
    function GetContributors: IStringList;
    ///  <summary>Returns list of testers of database.</summary>
    ///  <remarks>
    ///  <para>Return value is meaningless if the meta data does not come
    ///  from a supported database.</para>
    ///  <para>Method of IDBMetaData.</para>
    ///  </remarks>
    function GetTesters: IStringList;
    ///  <summary>Checks if meta data is recognised as belonging to a valid
    ///  database, whether supported or not.</summary>
    ///  <remarks>Method of IDBMetaData.</remarks>
    function IsRecognised: Boolean;
    ///  <summary>Checks if meta data is recognised as belonging to a supported
    ///  database version.</summary>
    ///  <remarks>Method of IDBMetaData.</remarks>
    function IsSupportedVersion: Boolean;
    ///  <summary>Checks if meta data is corrupt.</summary>
    ///  <remarks>
    ///  <para>Should only be called if meta data belongs to a supported
    ///  database.</para>
    ///  <para>Method of IDBMetaData.</para>
    ///  </remarks>
    ///  <exception>ENotSupportedException raised if called on an unsupported
    ///  database.</exception>
    function IsCorrupt: Boolean;
    ///  <summary>Refreshes the meta information by re-reading from database
    ///  meta files.</summary>
    ///  <remarks>Method of IDBMetaData.</summary>
    procedure Refresh;
  end;

  ///  <summary>Class that provides meta data for the main database.</summary>
  TMainDBMetaData = class sealed(TAbstractMainDBMetaData, IDBMetaData)
  strict private
    var
      fCollection: TCollection;
  strict protected
    function GetDBDir: string; override;
  public
    constructor Create(const ACollection: TCollection);
  end;

  ///  <summary>Class that provides meta data for update database directories.
  ///  </summary>
  TUpdateDBMetaData = class sealed(TAbstractMainDBMetaData, IDBMetaData)
  strict private
    var
      fUpdateDir: string;
  strict protected
    function GetDBDir: string; override;
  public
    constructor Create(const UpdateDir: string);
  end;

implementation

uses
  // Delphi
  Classes,
  IOUtils,
  // Project
  DB.DataFormats,
  UEncodings,
  UIOUtils,
  UResourceUtils,
  UStrUtils;

{ TAbstractMainDBMetaData }

procedure TAbstractMainDBMetaData.AfterConstruction;
begin
  inherited;
  Refresh;
end;

class function TAbstractMainDBMetaData.Capabilities: TMetaDataCapabilities;
begin
  Result := [mdcVersion, mdcLicense, mdcCopyright, mdcContributors, mdcTesters];
end;

destructor TAbstractMainDBMetaData.Destroy;
begin
  fMetaFiles.Free;
  inherited;
end;

function TAbstractMainDBMetaData.GetContributors: IStringList;
begin
  if not Assigned(fContributors) then
    fContributors := TIStringList.Create(fMetaFiles.Contributors);
  Result := fContributors;
end;

function TAbstractMainDBMetaData.GetCopyrightInfo: TDBCopyrightInfo;
begin
  if not fIsLicenseAndCopyrightInfoLoaded then
    LoadLicenseAndCopyrightInfo;
  Result := fCopyrightInfo;
end;

function TAbstractMainDBMetaData.GetLicenseInfo: TDBLicenseInfo;
begin
  if not fIsLicenseAndCopyrightInfoLoaded then
    LoadLicenseAndCopyrightInfo;
  Result := fLicenseInfo;
end;

function TAbstractMainDBMetaData.GetTesters: IStringList;
begin
  if not Assigned(fTesters) then
    fTesters := TIStringList.Create(fMetaFiles.Testers);
  Result := fTesters;
end;

function TAbstractMainDBMetaData.GetVersion: TVersionNumber;
begin
  if not fIsVersionLoaded then
  begin
    if not TVersionNumber.TryStrToVersionNumber(
      StrTrim(fMetaFiles.Version), fVersion
    ) then
      fVersion := TVersionNumber.Nul;
  end;
  fIsVersionLoaded := True;
  Result := fVersion;
end;

class function TAbstractMainDBMetaData.Instance(
  ACollection: DB.UCollections.TCollection): IDBMetaData;
begin
  Result := TMainDBMetaData.Create(ACollection);
end;

function TAbstractMainDBMetaData.IsCorrupt: Boolean;
resourcestring
  sNotSupportedError = 'Can''t call IDBMetaData.IsCorrupt for an unsupported '
    + 'database version';
begin
  if not IsSupportedVersion then
    raise ENotSupportedException.Create(sNotSupportedError);
  Result := not fMetaFiles.AreAllFilesPresent;
end;

function TAbstractMainDBMetaData.IsRecognised: Boolean;
begin
  Result := not GetVersion.IsNull;
end;

function TAbstractMainDBMetaData.IsSupportedVersion: Boolean;
var
  ThisVersion: TVersionNumber;
begin
  ThisVersion := GetVersion;
  Result := DBValidVersions.Contains(ThisVersion.V1);
end;

procedure TAbstractMainDBMetaData.LoadLicenseAndCopyrightInfo;
var
  SL: TStringList;
begin
  if fIsLicenseAndCopyrightInfoLoaded then
    Exit;
  SL := TStringList.Create;
  try
    StrArrayToStrList(fMetaFiles.LicenseInfo, SL);
    fLicenseInfo := TDBLicenseInfo.Create(
      SL.Values['LicenseName'],
      SL.Values['LicenseSPDX'],
      SL.Values['LicenseURL'],
      fMetaFiles.LicenseText
    );
    fCopyrightInfo := TDBCopyrightInfo.Create(
      SL.Values['CopyrightDate'],
      SL.Values['CopyrightHolder'],
      SL.Values['CopyrightHolderURL']
    );
  finally
    SL.Free;
  end;
  fIsLicenseAndCopyrightInfoLoaded := True;
end;

procedure TAbstractMainDBMetaData.Refresh;
begin
  FreeAndNil(fMetaFiles);
  fMetaFiles := TDBMetaFilesFactory.GetInstance(GetDBDir);
  fIsVersionLoaded := False;
  fIsLicenseAndCopyrightInfoLoaded := False;
  fContributors := nil;
  fTesters := nil;
end;

{ TMainDBMetaData }

constructor TMainDBMetaData.Create(
  const ACollection: DB.UCollections.TCollection);
begin
  inherited Create;
  fCollection := ACollection;
end;

function TMainDBMetaData.GetDBDir: string;
begin
  Result := fCollection.Location.Directory;
end;

{ TUpdateDBMetaData }

constructor TUpdateDBMetaData.Create(const UpdateDir: string);
begin
  inherited Create;
  fUpdateDir := ExcludeTrailingPathDelimiter(UpdateDir);
end;

function TUpdateDBMetaData.GetDBDir: string;
begin
  Result := fUpdateDir;
end;

{ TDBMetaFiles }

constructor TDBMetaFiles.Create(const DBDir: string);
begin
  inherited Create;
  fDBDir := DBDir;
end;

function TDBMetaFiles.DBFileExists(const FileName: string): Boolean;
begin
  Result := TFile.Exists(MakePath(FileName), False);
end;

function TDBMetaFiles.GetFileEncoding(const FileName: TFileName): TEncoding;
begin
  // Old v1 database meta files may be in the system default encodings, v1 and
  // all v2 and later use UTF-8 with BOM.
  if TFileIO.CheckBOM(MakePath(FileName), TEncoding.UTF8) then
    Result := TEncoding.UTF8
  else
    Result := TEncoding.Default;
end;

function TDBMetaFiles.MakePath(const FileName: string): string;
begin
  Assert(not StrContainsStr(PathDelim, FileName),
    ClassName + '.MakePath: FileName must be a base file name.');
  Result := IncludeTrailingPathDelimiter(fDBDir) + FileName;
end;

function TDBMetaFiles.ReadFileLines(const FileName: TFileName): TStringDynArray;
var
  Encoding: TEncoding;
begin
  if not DBFileExists(FileName) then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  Encoding := GetFileEncoding(FileName);
  try
    Result := TFileIO.ReadAllLines(MakePath(FileName), Encoding, True);
  finally
    TEncodingHelper.FreeEncoding(Encoding);
  end;
end;

function TDBMetaFiles.ReadFileText(const FileName: TFileName): string;
begin
  if not DBFileExists(FileName) then
    Exit('');
  Result := TFileIO.ReadAllText(
    MakePath(FileName), GetFileEncoding(FileName), True
  );
end;

{ TV1DBMetaFiles }

function TV1DBMetaFiles.AreAllFilesPresent: Boolean;
begin
  Result := DBFileExists(cContributorsFile) and DBFileExists(cTestersFile);
end;

function TV1DBMetaFiles.Contributors: TStringDynArray;
begin
  Result := ReadFileLines(cContributorsFile)
end;

function TV1DBMetaFiles.LicenseInfo: TStringDynArray;
begin
  Result := TStringDynArray.Create(
    'LicenseName=MIT License',
    'LicenseSPDX=MIT',
    'LicenseURL=https://opensource.org/licenses/MIT',
    'CopyrightDate=2005-2016',
    'CopyrightHolder=Peter Johnson & Contributors',
    'CopyrightHolderURL=https://gravatar.com/delphidabbler'
  );
end;

function TV1DBMetaFiles.LicenseText: string;
begin
  Result := LoadResourceAsString(HInstance, 'CSDBLICENSE', RT_RCDATA, etUTF8);
end;

function TV1DBMetaFiles.Testers: TStringDynArray;
begin
  Result := ReadFileLines(cTestersFile);
end;

function TV1DBMetaFiles.Version: string;
begin
  Result := '1';
end;

{ TV2DBMetaFiles }

function TV2DBMetaFiles.AreAllFilesPresent: Boolean;
begin
  Result := DBFileExists(cVersionFile)
    and DBFileExists(cLicenseFile)
    and DBFileExists(cLicenseInfoFile)
    and DBFileExists(cContributorsFile)
    and DBFileExists(cTestersFile);
end;

function TV2DBMetaFiles.Contributors: TStringDynArray;
begin
  Result := ReadFileLines(cContributorsFile);
end;

function TV2DBMetaFiles.LicenseInfo: TStringDynArray;
begin
  Result := ReadFileLines(cLicenseInfoFile);
end;

function TV2DBMetaFiles.LicenseText: string;
begin
  Result := StrTrimRight(ReadFileText(cLicenseFile));
end;

function TV2DBMetaFiles.Testers: TStringDynArray;
begin
  Result := ReadFileLines(cTestersFile);
end;

function TV2DBMetaFiles.Version: string;
begin
  Result := StrTrim(ReadFileText(cVersionFile));
end;

{ TLaterDBMetaFiles }

function TLaterDBMetaFiles.AreAllFilesPresent: Boolean;
resourcestring
  sNotSupportedError = 'Calling %s.AreAllFilesPresent is not supported for an '
    + 'unsupported database format';
begin
  raise ENotSupportedException.CreateFmt(sNotSupportedError, [ClassName]);
end;

function TLaterDBMetaFiles.Contributors: TStringDynArray;
begin
  SetLength(Result, 0);
end;

function TLaterDBMetaFiles.LicenseInfo: TStringDynArray;
begin
  SetLength(Result, 0);
end;

function TLaterDBMetaFiles.LicenseText: string;
begin
  Result := '';
end;

function TLaterDBMetaFiles.Testers: TStringDynArray;
begin
  SetLength(Result, 0);
end;

function TLaterDBMetaFiles.Version: string;
begin
  Result := StrTrim(ReadFileText(cVersionFile));
end;

{ TUnknownOrMissingMetaFiles }

function TUnknownOrMissingMetaFiles.AreAllFilesPresent: Boolean;
resourcestring
  sNotSupportedError = 'Calling %s.AreAllFilesPresent is not supported for an '
    + 'unrecognised database format or missing database';
begin
  raise ENotSupportedException.CreateFmt(sNotSupportedError, [ClassName]);
end;

function TUnknownOrMissingMetaFiles.Contributors: TStringDynArray;
begin
  SetLength(Result, 0);
end;

function TUnknownOrMissingMetaFiles.LicenseInfo: TStringDynArray;
begin
  SetLength(Result, 0);
end;

function TUnknownOrMissingMetaFiles.LicenseText: string;
begin
  Result := '';
end;

function TUnknownOrMissingMetaFiles.Testers: TStringDynArray;
begin
  SetLength(Result, 0);
end;

function TUnknownOrMissingMetaFiles.Version: string;
begin
  Result := '';
end;

{ TDBMetaFilesFactory }

class function TDBMetaFilesFactory.GetInstance(const DBDir: string):
  TDBMetaFiles;
var
  VersionFile: string;
  VersionStr: string;
  Version: TVersionNumber;
  DBPath: string;
begin
  if not TDirectory.Exists(ExcludeTrailingPathDelimiter(DBDir)) then
    // Database is not installed
    Exit(TUnknownOrMissingMetaFiles.Create(DBDir));

  DBPath := IncludeTrailingPathDelimiter(DBDir);

  // Check if VERSION file exists:
  //   Yes:
  //     either: version is invalid - database format unknown
  //     or: version is 2.x.x.x - database format v2 recognised
  //     or: version is >2 - database format recognised but not supported
  //   No:
  //     either: expected v1 meta files exist - database format v1 recognised
  //     or: no v1 meta files - database format unknown
  VersionFile := DBPath + TDBMetaFileNames.VersionV2AndLater;
  if TFile.Exists(VersionFile, False) then
  begin
    VersionStr := TFileIO.ReadAllText(VersionFile, TEncoding.UTF8, True);
    if not TVersionNumber.TryStrToVersionNumber(VersionStr, Version) then
      Result := TUnknownOrMissingMetaFiles.Create(DBDir)
    else if Version.V1 = 2 then
      Result := TV2DBMetaFiles.Create(DBDir)
    else
      Result := TLaterDBMetaFiles.Create(DBDir);
  end
  else
  begin
    if TFile.Exists(DBPath + TDBMetaFileNames.ContributorsV1, False)
      and TFile.Exists(DBPath + TDBMetaFileNames.TestersV1, False) then
      Result := TV1DBMetaFiles.Create(DBDir)
    else
      Result := TUnknownOrMissingMetaFiles.Create(DBDir);
  end;
end;

initialization

TMetaDataFactory.RegisterCreator(
  TDataFormatKind.DCSC_v2, TMainDBMetaData
);

end.
