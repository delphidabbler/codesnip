{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements code that reads the main CodeSnip database from .ini and .dat
 * files.
}

{TODO -cVault: rename INI term to DCSCv2 - this isn't a general .ini data
        IO unit, the .ini format only part of a wider vault format.}

unit DBIO.UIniData;


interface


uses
  // Delphi
  SysUtils,
  Classes,
  Types,
  Generics.Collections,
  Generics.Defaults,
  IniFiles,
  // Project
  ActiveText.UMain,
  DB.MetaData,
  DB.UCategory,
  DB.USnippet,
  DBIO.UFileIOIntf,
  UIStringList,
  UVersionInfo;


type

  ///  <summary>
  ///  Reads main CodeSnip database data from .ini and .dat files.
  ///  </summary>
  TIniDataReader = class sealed(TInterfacedObject, IDataReader)
  strict private
    type
      ///  <summary>
      ///  Class that implements a cache of ini file objects, indexed by ini
      ///  file name.
      ///  </summary>
      TIniFileCache = class(TObject)
      strict private
        type
          ///  <summary>
          ///  Class that maps ini file names to related ini file objects.
          ///  </summary>
          TIniFileMap = TObjectDictionary<string,TCustomIniFile>;
        var
          ///  <summary>Maps file names to related ini file objects.</summary>
          fCache: TIniFileMap;
      public
        ///  <summary>Object constructor. Sets up empty cache.</summary>
        constructor Create;
        ///  <summary>Object destructor. Frees cache.</summary>
        destructor Destroy; override;
        ///  <summary>
        ///  Gets reference to ini file object. Creates it if it doesn't extist.
        ///  </summary>
        ///  <param name="PathToFile">string [in] Fully specified path to ini
        ///  file.</param>
        ///  <returns>TCustomIniFile instance for reading ini file.</returns>
        ///  <remarks>Caller must not free the returned TCustomIniFile instance.
        ///  </remarks>
        function GetIniFile(const PathToFile: string): TCustomIniFile;
      end;
    type
      ///  <summary>Class that maps snippet names to category ids.</summary>
      TSnippetCatMap = TDictionary<string,Integer>;
    var
      ///  <summary>Database directory.</summary>
      fDBDir: string;
      ///  <summary>Reference to master ini file.</summary>
      fMasterIni: TCustomIniFile;
      ///  <summary>List of category ids in database.</summary>
      fCatIDs: TStringList;
      ///  <summary>Map of snippet names to category ids.</summary>
      fSnippetCatMap: TSnippetCatMap;
      ///  <summary>Cache of category ini file objects.</summary>
      fIniCache: TIniFileCache;
      ///  <summary>Data format version number.</summary>
      fVersion: TVersionNumber;
    const
      SupportedMajorVersion = 2;
  strict private
    ///  <summary>
    ///  Returns fully specified name of database master file.
    ///  </summary>
    function MasterFileName: string;
    ///  <summary>Reads data format version from file.</summary>
    ///  <remarks>Stores result in <c>fVersion</c>. Detects DCSC versions 1 and
    ///  later. <c>fVersion</c> is set to null if the version can't be
    ///  determined.</remarks>
    procedure ReadVersionNumber;
    ///  <summary>
    ///  Returns ID of category associated with a snippet.
    ///  </summary>
    ///  <param name="SnippetKey">string [in] Snippet's key.</param>
    ///  <returns>string containing category ID</returns>
    function SnippetToCat(const SnippetKey: string): string;
    ///  <summary>
    ///  Returns name of ini file containing details of a category.
    ///  </summary>
    ///  <param name="CatID">string [in] Id of category.</param>
    ///  <returns>string containing name of category's ini file</returns>
    function CatToCatIni(const CatID: string): string;
    ///  <summary>
    ///  Loads indices of all names of categories and snippets in database.
    ///  </summary>
    ///  <remarks>
    ///  Having these indices available speeds up several of the main methods.
    ///  </remarks>
    procedure LoadIndices;
    ///  <summary>
    ///  Handles exceptions raised when a corrupt database is encountered.
    ///  Deletes all files and re-raises exception.
    ///  </summary>
    ///  <param name="EObj">Exception object to be handled.</param>
    procedure HandleCorruptDatabase(const EObj: TObject);
    ///  <summary>Returns encoding used by given meta file.</summary>
    function GetFileEncoding(const FileName: string): TEncoding;
    ///  <summary>
    ///  Returns name of directory where the database is stored.
    ///  </summary>
    function DataDir: string;
    ///  <summary>
    ///  Returns fully specified path to given file name.
    ///  </summary>
    function DataFile(const FileName: string): string;
    ///  <summary>Checks if a given file exists in the vault directory.
    ///  </summary>
    function DataFileExists(const FileName: string): Boolean;
    ///  <summary>Reads all lines from given file and returns them as an array.
    ///  </summary>
    ///  <remarks>FileName must contain no path information.</remarks>
    function ReadFileLines(const FileName: string): TStringDynArray;
    ///  <summary>Reads all text from given file and returns it.</summary>
    ///  <remarks>FileName must contain no path information.</remarks>
    function ReadFileText(const FileName: string): string;
    ///  <summary>
    ///  Gets a list from ini file of all of items of a specified kind that are
    ///  referenced by a snippet.
    ///  </summary>
    ///  <param name="SnippetKey">string [in] Snippet's key.</param>
    ///  <param name="KeyName">string [in] Name of a key in ini file storing
    ///  comma separated list of references.</param>
    ///  <returns>IStringList containing names of referenced items.</returns>
    function GetSnippetReferences(const SnippetKey, KeyName: string): IStringList;
  strict protected
    ///  <summary>
    ///  Extracts comma delimited text fields into a string list.
    ///  </summary>
    ///  <param name="CommaStr">string [in] Comma delimited text.</param>
    ///  <returns>IStringList containing fields.</returns>
    class function CommaStrToStrings(const CommaStr: string): IStringList;
  public
    ///  <summary>
    ///  Object constructor. Checks if database exists and sets up indices.
    ///  </summary>
    ///  <param name="DBDir">string [in] Directory containing database.</param>
    constructor Create(const DBDir: string);
    ///  <summary>
    ///  Object destructor. Tears down object.
    ///  </summary>
    destructor Destroy; override;
    { IDataReader methods }
    ///  <summary>
    ///  Checks if the database exists.
    ///  </summary>
    ///  <remarks>
    ///  This method is always called before any other IDataReader methods. The
    ///  other methods are not called if this method returns False.
    ///  </remarks>
    function DatabaseExists: Boolean;
    ///  <summary>
    ///  Gets name of all categories in the database.
    ///  </summary>
    ///  <returns>IStringList containing names.</returns>
    function GetAllCatIDs: IStringList;
    ///  <summary>
    ///  Gets properties of a category.
    ///  </summary>
    ///  <param name="CatID">string [in] Id of category.</param>
    ///  <param name="Props">TCategoryData [in/out] Receives empty property
    ///  record and updates relevant property fields.</param>
    procedure GetCatProps(const CatID: string; var Props: TCategoryData);
    ///  <summary>
    ///  Gets keys of all snippets in a category.
    ///  </summary>
    ///  <param name="CatID">string [in] Id of category.</param>
    ///  <returns>IStringList containing keys of snippets.</returns>
    function GetCatSnippets(const CatID: string): IStringList;
    ///  <summary>
    ///  Gets properties of a snippet.
    ///  </summary>
    ///  <param name="SnippetKey">string [in] Snippet's key.</param>
    ///  <param name="Props">TSnippetData [in/out] Receives empty property
    ///  record and updates relevant property fields.</param>
    procedure GetSnippetProps(const SnippetKey: string;
      var Props: TSnippetData);
    ///  <summary>
    ///  Gets list of all snippets that are cross referenced by a specified
    ///  snippet.
    ///  </summary>
    ///  <param name="SnippetKey">string [in] Snippet's key.</param>
    ///  <returns>IStringList containing snippet keys.</returns>
    function GetSnippetXRefs(const SnippetKey: string): IStringList;
    ///  <summary>
    ///  Gets list of all snippets on which a specified snippet depends.
    ///  </summary>
    ///  <param name="SnippetKey">string [in] Snippet's key.</param>
    ///  <returns>IStringList containing snippet keys.</returns>
    function GetSnippetDepends(const SnippetKey: string): IStringList;
    ///  <summary>
    ///  Gets list of all units referenced by a snippet.
    ///  </summary>
    ///  <param name="SnippetKey">string [in] Snippet's key.</param>
    ///  <returns>IStringList containing unit names.</returns>
    function GetSnippetUnits(const SnippetKey: string): IStringList;
    ///  <summary>Gets the vault's meta data.</summary>
    ///  <returns><c>TMetaData</c>. The required meta data. Will be null if
    ///  is no meta data present.</returns>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    function GetMetaData: TMetaData;
  end;

  ///  <summary>Write a vault to disk in the DelphiDabbler Code Snippets
  ///  Collection v2 format.</summary>
  TIniDataWriter = class sealed(TInterfacedObject, IDataWriter)
  strict private
    type

      ///  <summary>Encapsulates an INI file stored in UTF8 format.</summary>
      TUTF8IniFile = class(TMemIniFile)
      public
        ///  <summary>Saves the ini data to a file in UTF8 format.</summary>
        procedure Save;
      end;

      ///  <summary>Implements a cache of UTF8 format ini file objects, indexed
      ///  by ini file name.</summary>
      TUTF8IniFileCache = class(TObject)
      strict private
        type
          ///  <summary>Maps ini file names to related ini file objects.
          ///  </summary>
          TIniFileMap = TObjectDictionary<string,TUTF8IniFile>;
        var
          ///  <summary>Maps file names to related ini file objects.</summary>
          fCache: TIniFileMap;

        ///  <summary>Adds an ini file to the cache and returns it.</summary>
        ///  <remarks>Performs no checks on whether the ini file is already in
        ///  the cache.</remarks>
        ///  <param name="APathToFile"><c>string</c> [in] Fully specified path
        ///  to ini file.</param>
        ///  <returns><c>TUTF8IniFile</c> instance.</returns>
        function InternalAddIniFile(const APathToFile: string): TUTF8IniFile;

      public

        ///  <summary>Object constructor. Sets up empty cache.</summary>
        constructor Create;

        ///  <summary>Object destructor. Frees cache.</summary>
        destructor Destroy; override;

        ///  <summary>Gets reference to ini file object. Creates it if i
        ///  doesn't extist.</summary>
        ///  <param name="APathToFile"><c>string</c> [in] Fully specified path
        ///  to ini file.</param>
        ///  <returns><c>TUTF8IniFile</c> instance.</returns>
        ///  <remarks>Caller must not free the returned <c>TCustomIniFile</c>
        ///  instance.</remarks>
        function GetIniFile(const APathToFile: string): TUTF8IniFile;

        ///  <summary>Creates and adds an ini file object to the cache if not
        ///  already present.</summary>
        ///  <param name="APathToFile"><c>string</c> [in] Fully specified path
        ///  to ini file.</param>
        procedure AddIniFile(const APathToFile: string);

        ///  <summary>Enumerator for cached ini files.</summary>
        function GetEnumerator:
          TObjectDictionary<string,TUTF8IniFile>.TPairEnumerator;
      end;

    var
      ///  <summary>Cache of ini files.</summary>
      fCache: TUTF8IniFileCache;
      ///  <summary>Output directory.</summary>
      fOutDir: string;
      ///  <summary>Path to master ini file.</summary>
      fMasterIniPath: string;
      ///  <summary>Ini file containing currently processed category.</summary>
      fCurrentCatIni: TUTF8IniFile;
      ///  <summary>Number of next available unused data file.</summary>
      fFileNumber: Integer;

    ///  <summary>Handles exceptions raised by converting expected exceptions
    ///  into <c>ECodeSnip</c> derived exceptions.</summary>
    ///  <param name="EObj"><c>TObject</c> [in] Reference to exception to be
    ///  handled.</param>
    ///  <exceptions>Always raise an exception.</exceptions>
    ///  <remarks>Unexpected exceptions are re-raised as is.</remarks>
    procedure HandleException(const EObj: TObject);

    ///  <summary>Returns full path to <c>AFileName</c> rooted at
    ///  <c>fOutDir</c>.</summary>
    function MakePath(const AFileName: string): string;

    ///  <summary>Returns the name of the ini file associated with
    ///  <c>ACatID</c>.</summary>
    function MakeCatIniName(const ACatID: string): string;

    ///  <summary>Returns the full path to the ini file associated with
    ///  <c>ACatID</c> rooted at <c>fOutDir</c>.</summary>
    function MakeCatIniPath(const ACatID: string): string;

    ///  <summary>Converts the given active text <c>AActiveText</c> to an
    ///  unformatted, single line, REML string.</summary>
    function ActiveTextToREML(AActiveText: IActiveText): string;

    procedure WriteTextFile(const AFileName, AText: string); overload;

    procedure WriteTextFile(const AFileName: string; const ALines: IStringList);
      overload;

  public

    ///  <summary>Object constructor.</summary>
    ///  <param name="AOutDir"><c>string</c> [in] Directory containing the
    ///  output.</param>
    constructor Create(const AOutDir: string);

    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;

    ///  <summary>Initialise the output. Always called before any other methods.
    ///  </summary>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure Initialise;

    ///  <summary>Write the properties of a category. Always called before
    ///  <c>WriteCatSnippets</c> for a given category, so can be used to perform
    ///  any per-category initialisation.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of category.</param>
    ///  <param name="Props"><c>TCategoryData</c> [in] Properties of category.
    ///  </param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteCatProps(const CatID: string; const Props: TCategoryData);

    ///  <summary>Write the list of snippet keys belonging to a category. Always
    ///  called after <c>WriteCatProps</c> for any given category.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of category.</param>
    ///  <param name="SnipList"><c>IStringList</c> [in] List of snippet keys.
    ///  </param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteCatSnippets(const CatID: string;
      const SnipList: IStringList);

    ///  <summary>Write the properties of a snippet. Always called after all
    ///  categories are written and before <c>WriteSnippetUnits</c>, so can be
    ///  used to perform any per-snippet intialisation.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's key.</param>
    ///  <param name="Props"><c>TSnippetData</c> [in] Properties of snippet.
    ///  </param>
    ///  <remarks>
    ///  <para>NOTE: This method conforms to DelphiDabbler Code Snippets
    ///  Collection format v2.1.x.</para>
    ///  <para>Method of <c>IDataWriter</c>.</para>
    ///  </remarks>
    procedure WriteSnippetProps(const SnippetKey: string;
      const Props: TSnippetData);

    ///  <summary>Write the list of units required by a snippet.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's key.</param>
    ///  <param name="Units"><c>IStringList</c> [in] List of names of required
    ///  units.</param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteSnippetUnits(const SnippetKey: string;
      const Units: IStringList);

    ///  <summary>Write the list of snippets on which a snippet depends.
    ///  </summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's key.</param>
    ///  <param name="Depends"><c>IStringList</c> [in] List of snippet keys.
    ///  </param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteSnippetDepends(const SnippetKey: string;
      const Depends: IStringList);

    ///  <summary>Write the list of snippets that a snippet cross-references.
    ///  </summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's keys.</param>
    ///  <param name="XRefs"><c>IStringList</c> [in] List of snippet keys.
    ///  </param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteSnippetXRefs(const SnippetKey: string;
      const XRefs: IStringList);

    ///  <summary>Writes the vault's meta data.</summary>
    ///  <param name="AMetaData"><c>TMetaData</c> [in] Meta data to be written.
    ///  </param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteMetaData(const AMetaData: TMetaData);

    ///  <summary>Finalises the output. Always called after all other methods.
    ///  </summary>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure Finalise;

  end;

implementation


uses
  // Delphi
  IOUtils,
  // Project
  Compilers.UGlobals,
  DB.USnippetKind,
  UComparers,
  UConsts,
  UEncodings,
  UExceptions,
  UIniDataLoader,
  UIOUtils,
  UREMLDataIO,
  USnippetExtraHelper,
  USystemInfo,
  UStrUtils,
  UUtils;


const
  // Name of master file that defines database
  cMasterFileName = 'categories.ini';

  // Names of v2 meta data files
  VersionFileName = 'VERSION';
  LicenseFileName = 'LICENSE';
  LicenseInfoFileName = 'LICENSE-INFO';
  ContributorsFileName = 'CONTRIBUTORS';
  AcknowledgementsFileName = 'TESTERS';

  // Names of v1 meta data files
  ContributorsFileNameV1 = 'contrib.txt';
  AcknowledgementsFileNameV1 = 'testers.txt';


  // Names of keys in license info file
  LicenseInfoLicenseNameKey = 'LicenseName';
  LicenseInfoLicenseSPDXKey = 'LicenseSPDX';
  LicenseInfoLicenseURLKey = 'LicenseURL';
  LicenseInfoCopyrightDateKey = 'CopyrightDate';
  LicenseInfoCopyrightHolderKey = 'CopyrightHolder';
  LicenseInfoCopyrightHolderURLKey = 'CopyrightHolderURL';

  // Names of values in categories ini file
  cMasterIniName = 'Ini';             // name of category ini file
  cMasterDescName = 'Desc';           // category description
  // Names of values in snippet sections of various category ini files
  cDependsName = 'Depends';           // dependency list for snippet
  cUnitsName = 'Units';               // required unit list for snippet
  cXRefName = 'SeeAlso';              // cross-reference list for snippet
  cDisplayName = 'DisplayName';       // snippet's display name if any
  cExtraName = 'Extra';               // extra information for snippet
  cCreditsName = 'Credits';           // snippet credits
  cCreditsURLName = 'Credits_URL';    // url relating to snippet credits
  cCommentsName = 'Comments';         // snippet additional comments
  cDescName = 'Desc';                 // snippet description (plain text)
  cDescExName = 'DescEx';             // snippet descriptio (REML)
  cSnipFileName = 'Snip';             // name of snippet's snippet file
  cStdFormatName = 'StandardFormat';  // whether snippet in std format
  cKindName = 'Kind';                 // kind of snippet
  cTestInfoName = 'TestInfo';         // snippet's testing information
  cCompilerIDNames:                   // snippet's compiler results for each
    array[TCompilerID] of string = (
    'Delphi2', 'Delphi3', 'Delphi4', 'Delphi5', 'Delphi6', 'Delphi7',
    'Delphi2005Win32', 'Delphi2006Win32', 'Delphi2007', 'Delphi2009Win32',
    'Delphi2010', 'DelphiXE', 'DelphiXE2', 'DelphiXE3', 'DelphiXE4',
    'DelphiXE5', 'DelphiXE6', 'DelphiXE7', 'DelphiXE8', 'Delphi10S',
    'Delphi101B', 'Delphi102T', 'Delphi103R', 'Delphi104S', 'Delphi11A',
    'Delphi12A',
    'FPC'
  );

{ TIniDataReader }

function TIniDataReader.CatToCatIni(const CatID: string): string;
begin
  Result := DataFile(fMasterIni.ReadString(CatID, cMasterIniName, ''));
end;

class function TIniDataReader.CommaStrToStrings(
  const CommaStr: string): IStringList;
begin
  Result := TIStringList.Create(CommaStr, ',', False, True);
end;

constructor TIniDataReader.Create(const DBDir: string);
resourcestring
  // Error messages
  sVersionNotSpecified = 'Format version number not specified';
  sVersionNotSupported = 'Format version %s is not supported';
begin
  inherited Create;
  fDBDir := DBDir;
  // Create helper objects used to speed up access to ini files
  if DatabaseExists then
  begin
    fIniCache := TIniFileCache.Create;
    try
      ReadVersionNumber;
      if fVersion.IsNull then
        raise EDataIO.Create(sVersionNotSpecified);
      if fVersion.V1 <> SupportedMajorVersion then
        raise EDataIO.CreateFmt(sVersionNotSupported, [string(fVersion)]);
      fMasterIni := TDatabaseIniFile.Create(MasterFileName);
      fCatIDs := TStringList.Create;
      fSnippetCatMap := TSnippetCatMap.Create(TTextEqualityComparer.Create);
      // Load required indexes
      LoadIndices;
    except
      HandleCorruptDatabase(ExceptObject);
    end;
  end;
end;

function TIniDataReader.DatabaseExists: Boolean;
begin
  Result := FileExists(MasterFileName);
end;

function TIniDataReader.DataDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(fDBDir)
end;

function TIniDataReader.DataFile(const FileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(DataDir) + FileName;
end;

function TIniDataReader.DataFileExists(const FileName: string): Boolean;
begin
  Result := TFile.Exists(DataFile(FileName), False);
end;

destructor TIniDataReader.Destroy;
begin
  fIniCache.Free;
  fSnippetCatMap.Free;
  fCatIDs.Free;
  fMasterIni.Free;
  inherited;
end;

function TIniDataReader.GetAllCatIDs: IStringList;
begin
  Result := TIStringList.Create(fCatIDs);
end;

procedure TIniDataReader.GetCatProps(const CatID: string;
  var Props: TCategoryData);
begin
  try
    Props.Desc := fMasterIni.ReadString(CatID, cMasterDescName, '');
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetCatSnippets(const CatID: string): IStringList;
var
  CatIniFile: string;
  CatIni: TCustomIniFile; // accesses .ini file associated with category
  SnipList: TStringList;  // list of snippets in category
begin
  Result := TIStringList.Create;
  try
    // Snippet names are names of sections in category's .ini file
    CatIniFile := CatToCatIni(CatID);
    if not TFile.Exists(CatIniFile) then
      // This is not an error since it is possible that a category exists in
      // another vault and loader will request info from that vault too.
      Exit;
    CatIni := fIniCache.GetIniFile(CatToCatIni(CatID));
    SnipList := TStringList.Create;
    try
      CatIni.ReadSections(SnipList);
      Result.Add(SnipList);
    finally
      SnipList.Free;
    end;
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetFileEncoding(const FileName: string): TEncoding;
begin
  // Old v1 database meta files may be in the system default encodings, v1 and
  // all v2 and later use UTF-8 with BOM.
  if TFileIO.CheckBOM(DataFile(FileName), TEncoding.UTF8) then
    Result := TEncoding.UTF8
  else
    Result := TEncoding.Default;
end;

function TIniDataReader.GetMetaData: TMetaData;
var
  SL: TStringList;
  LicenseText: string;
  LicenseFileInfo: TStringDynArray;
  Contributors: IStringList;
begin

  LicenseText := StrTrimRight(ReadFileText(LicenseFileName));
  LicenseFileInfo := ReadFileLines(LicenseInfoFileName);
  Contributors := TIStringList.Create(ReadFileLines(ContributorsFileName));

  Result := TMetaData.Create([
    TMetaDataCap.Version, TMetaDataCap.License, TMetaDataCap.Copyright,
    TMetaDataCap.Acknowledgements
  ]);
  Result.Version := fVersion; // this was read in constructor
  SL := TStringList.Create;
  try
    StrArrayToStrList(LicenseFileInfo, SL);
    Result.LicenseInfo := TLicenseInfo.Create(
      SL.Values[LicenseInfoLicenseNameKey],
      SL.Values[LicenseInfoLicenseSPDXKey],
      SL.Values[LicenseInfoLicenseURLKey],
      LicenseText
    );
    Result.CopyrightInfo := TCopyrightInfo.Create(
      SL.Values[LicenseInfoCopyrightDateKey],
      SL.Values[LicenseInfoCopyrightHolderKey],
      SL.Values[LicenseInfoCopyrightHolderURLKey],
      Contributors
    );
  finally
    SL.Free;
  end;
  Result.Acknowledgements := TIStringList.Create(
    ReadFileLines(AcknowledgementsFileName)
  );
end;

function TIniDataReader.GetSnippetDepends(const SnippetKey: string):
  IStringList;
begin
  Result := GetSnippetReferences(SnippetKey, cDependsName);
end;

procedure TIniDataReader.GetSnippetProps(const SnippetKey: string;
  var Props: TSnippetData);
var
  CatIni: TCustomIniFile; // .ini file associated with snippet's category
  CatID: string;          // snippet's category id

  // ---------------------------------------------------------------------------
  /// <summary>Reads "StandardFormat" value from ini file.</summary>
  function GetStdFormatProperty: Boolean;
  begin
    Result := CatIni.ReadBool(SnippetKey, cStdFormatName, True);
  end;

  ///  <summary>Reads "Kind" value from ini file.</summary>
  function GetKindProperty: TSnippetKind;
  var
    KindStr: string;  // string value read from ini file
  begin
    KindStr := CatIni.ReadString(SnippetKey, cKindName, '');
    if StrSameText(KindStr, 'freeform') then
      Result := skFreeform
    else if StrSameText(KindStr, 'routine') then
      Result := skRoutine
    else if StrSameText(KindStr, 'const') then
      Result := skConstant
    else if StrSameText(KindStr, 'type') then
      Result := skTypeDef
    else if StrSameText(KindStr, 'unit') then
      Result := skUnit
    else if StrSameText(KindStr, 'class') then
      Result := skClass
    // invalid or no Kind property: kind depends on StdFormat property
    else if GetStdFormatProperty then
      Result := skRoutine
    else
      Result := skFreeform;
  end;

  ///  <summary>Reads "Extra" value from ini file and converts to active text.
  ///  </summary>
  function GetExtraProperty: IActiveText;
  var
    Extra: string;  // extra value from ini file if present
  begin
    try
      Extra := CatIni.ReadString(SnippetKey, cExtraName, '');
      if Extra <> '' then
        // There is an "extra" value: use it to set Extra property. We ignore
        // any credits, credits url and comments values in this case
        Result := TSnippetExtraHelper.BuildActiveText(Extra)
      else
        // There is no "extra" value: use any comments, credits and credits URL
        // values to set Extra property
        Result := TSnippetExtraHelper.BuildActiveText(
          CatIni.ReadString(SnippetKey, cCommentsName, ''),
          CatIni.ReadString(SnippetKey, cCreditsName, ''),
          CatIni.ReadString(SnippetKey, cCreditsURLName, '')
        );
    except
      // There was an error: use an empty property value
      Result := TActiveTextFactory.CreateActiveText;
    end;
  end;

  ///  <summary>Reads "Snip" value from ini value and loads source code from the
  ///  referenced file.</summary>
  function GetSourceCodeProperty: string;
  var
    SnipFileName: string; // name of file containing source code
  begin
    SnipFileName := CatIni.ReadString(SnippetKey, cSnipFileName, '');
    try
      Result := TFileIO.ReadAllText(
        DataFile(SnipFileName), TEncoding.UTF8, True
      );
    except
      // if error loading file then database is corrupt
      on E: EFOpenError do
        raise EDataIO.Create(E);
      else
        raise;
    end;
  end;

  ///  <summary>Reads all compiler ID values from ini file and builds list of
  ///  compiler results.</summary>
  function GetCompilerResultsProperty: TCompileResults;
  var
    CompID: TCompilerID;    // loops thru supported compilers
    CompRes: string;        // character indicating compiler result
  begin
    for CompID := Low(TCompilerID) to High(TCompilerID) do
    begin
      CompRes := CatIni.ReadString(SnippetKey, cCompilerIDNames[CompID], '?');
      if CompRes = '' then
        CompRes := '?';
      case CompRes[1] of
        'W', // warning result now treated as success
        'Y': Result[CompID] := crSuccess;
        'N': Result[CompID] := crError;
        else Result[CompID] := crQuery;
      end;
    end;
  end;

  ///  <summary>Gets snippet description from ini file.</summary>
  ///  <remarks>Uses REML from DescEx field if present, otherwise uses plain
  ///  text from Desc field if present, otherwise description is empty.
  ///  </remarks>
  function GetDescription: IActiveText;
  var
    REML: string;       // REML code from DescEx field
    PlainText: string;  // plain text from Desc field
  begin
    REML := CatIni.ReadString(SnippetKey, cDescExName, '');
    if REML <> '' then
      Result := TSnippetExtraHelper.BuildActiveText(REML)
    else
    begin
      PlainText := CatIni.ReadString(SnippetKey, cDescName, '');
      if PlainText <> '' then
        Result := TSnippetExtraHelper.PlainTextToActiveText(PlainText)
      else
        Result := TActiveTextFactory.CreateActiveText;
    end;
  end;

  ///  <summary>Gets snippet's display name from ini file.</summary>
  function GetDisplayNameProperty: string;
  begin
    Result := CatIni.ReadString(SnippetKey, cDisplayName, '');
  end;

  ///  <summary>Get's snippet's test info from ini file.</summary>
  function GetTestInfoProperty: TSnippetTestInfo;
  var
    Str: string;  // string value read from ini file
  begin
    Str := CatIni.ReadString(SnippetKey, cTestInfoName, 'basic');
    if StrSameText(Str, 'basic') then
      Result := stiBasic
    else if StrSameText(Str, 'advanced') then
      Result := stiAdvanced
    else // Str = 'none' or any invalid value
      Result := stiNone;
  end;
  // ---------------------------------------------------------------------------

begin
  try
    // Get name of category associated with this snippet
    CatID := SnippetToCat(SnippetKey);
    // Get snippet properties from values listed under snippet's section in
    // category's .ini file
    CatIni := fIniCache.GetIniFile(CatToCatIni(CatID));
    Props.Kind := GetKindProperty;
    Props.Cat := CatID;
    Props.Desc := GetDescription;
    Props.Extra := GetExtraProperty;
    Props.DisplayName := GetDisplayNameProperty;
    Props.SourceCode := GetSourceCodeProperty;
    Props.CompilerResults := GetCompilerResultsProperty;
    Props.TestInfo := GetTestInfoProperty;
    // all snippets from main database are Pascal and use syntax highlighter:
    // there is no entry in data files to switch this on or off
    Props.HiliteSource := True;
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetSnippetReferences(const SnippetKey,
  KeyName: string): IStringList;
var
  CatIni: TCustomIniFile; // accesses snippet's category's .ini
begin
  try
    // References are contained in comma separated value in category's ini file
    // under snippet's section
    CatIni := fIniCache.GetIniFile(CatToCatIni(SnippetToCat(SnippetKey)));
    Result := CommaStrToStrings(CatIni.ReadString(SnippetKey, KeyName, ''));
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetSnippetUnits(const SnippetKey: string): IStringList;
begin
  Result := GetSnippetReferences(SnippetKey, cUnitsName);
end;

function TIniDataReader.GetSnippetXRefs(const SnippetKey: string): IStringList;
begin
  Result := GetSnippetReferences(SnippetKey, cXRefName);
end;

procedure TIniDataReader.HandleCorruptDatabase(const EObj: TObject);
resourcestring
  // Error message
  sDBError = 'The database is corrupt and had been deleted.' + EOL2 + '%s';
begin
  DeleteFiles(DataDir, '*.*');
  if (EObj is EDataIO)
    or (EObj is EFileStreamError)
    or (EObj is EDatabaseIniFile) then
    // we have database error: raise new exception containing old message
    raise EDataIO.CreateFmt(sDBError, [(EObj as Exception).Message])
  else
    // not an ECodeSnip: just re-raise
    raise EObj;
end;

procedure TIniDataReader.LoadIndices;
var
  SnippetKey: string;         // key of each snippet in a category
  CatIdx: Integer;            // loops thru all categories
  CatSnippets: IStringList;   // list of snippets in a single category
begin
  // Read in list of category names
  fMasterIni.ReadSections(fCatIDs);
  // We build map of snippet names to categories by reading snippets in each
  // category and referencing that category's id with the snippet name.
  CatSnippets := TIStringList.Create;
  for CatIdx := 0 to Pred(fCatIDs.Count) do
  begin
    // Get list of snippets in category ...
    CatSnippets := GetCatSnippets(fCatIDs[CatIdx]);
    for SnippetKey in CatSnippets do
      fSnippetCatMap.Add(SnippetKey, CatIdx);
  end;
end;

function TIniDataReader.MasterFileName: string;
begin
  Result := DataFile(cMasterFileName);
end;

function TIniDataReader.ReadFileLines(const FileName: string): TStringDynArray;
var
  Encoding: TEncoding;
begin
  if not DataFileExists(FileName) then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  Encoding := GetFileEncoding(FileName);
  try
    Result := TFileIO.ReadAllLines(DataFile(FileName), Encoding, True);
  finally
    TEncodingHelper.FreeEncoding(Encoding);
  end;
end;

function TIniDataReader.ReadFileText(const FileName: string): string;
begin
  if not DataFileExists(FileName) then
    Exit('');
  Result := TFileIO.ReadAllText(
    DataFile(FileName), GetFileEncoding(FileName), True
  );
end;

procedure TIniDataReader.ReadVersionNumber;
var
  VersionStr: string;
begin
  if DataFileExists(VersionFileName) then
  begin
    // Version file exists. Read and parse it. Set to null if invalid
    VersionStr := StrTrim(ReadFileText(VersionFileName));
    if not TVersionNumber.TryStrToVersionNumber(VersionStr, fVersion) then
      fVersion := TVersionNumber.Nul;
  end
  else
  begin
    // No version file. Check if v1 present. Set to null if v1 not detected
    if DataFileExists(ContributorsFileNameV1)
      and DataFileExists(AcknowledgementsFileNameV1) then
      fVersion := TVersionNumber.Create(1, 0, 0, 0)
    else
      fVersion := TVersionNumber.Nul;
  end;
end;

function TIniDataReader.SnippetToCat(const SnippetKey: string): string;
var
  CatIdx: Integer;  // index of category in category list for this snippet
resourcestring
  // Error message
  sMissingSnippet = 'Snippet key "%s" not found in database.';
begin
  if not fSnippetCatMap.ContainsKey(SnippetKey) then
    raise EDataIO.CreateFmt(sMissingSnippet, [SnippetKey]);
  CatIdx := fSnippetCatMap[SnippetKey];
  Result := fCatIDs[CatIdx];
end;

{ TIniDataReader.TIniFileCache }

constructor TIniDataReader.TIniFileCache.Create;
begin
  inherited Create;
  // fCache owns and frees the ini file objects
  fCache := TIniFileMap.Create(
    [doOwnsValues], TTextEqualityComparer.Create
  );
end;

destructor TIniDataReader.TIniFileCache.Destroy;
begin
  fCache.Free;  // frees owned .Values[] objects
  inherited;
end;

function TIniDataReader.TIniFileCache.GetIniFile(
  const PathToFile: string): TCustomIniFile;
begin
  if not fCache.ContainsKey(PathToFile) then
    fCache.Add(PathToFile, TDatabaseIniFile.Create(PathToFile));
  Result := fCache[PathToFile];
end;

{ TIniDataWriter }

function TIniDataWriter.ActiveTextToREML(AActiveText: IActiveText): string;
begin
  Result := TREMLWriter.Render(AActiveText, False);
end;

constructor TIniDataWriter.Create(const AOutDir: string);
begin
  inherited Create;
  fOutDir := AOutDir;
  fCache := TUTF8IniFileCache.Create;
end;

destructor TIniDataWriter.Destroy;
begin
  fCache.Free;  // frees owned ini file objects
  inherited;
end;

procedure TIniDataWriter.Finalise;
var
  IniInfo: TPair<string,TUTF8IniFile>;
begin
  try
    for IniInfo in fCache do
      IniInfo.Value.Save;
  except
    HandleException(ExceptObject);
  end;
end;

procedure TIniDataWriter.HandleException(const EObj: TObject);
begin
  if (EObj is EFileStreamError) or (EObj is ECodeSnip)
    or (EObj is EDirectoryNotFoundException) then
    raise EDataIO.Create(EObj as Exception);
  raise EObj;
end;

procedure TIniDataWriter.Initialise;
begin
  try
    // Make sure database folder exists
    TDirectory.CreateDirectory(fOutDir);

    // Delete current ini and data files
    // (don't delete special files: CONTRIBUTORS, LICENSE, LICENSE-INFO,
    // TESTERS, VERSION).
    {TODO -cVault: Is it now safe to delete the special files, since we now
          write these files.}
    DeleteFiles(fOutDir, '*.dat');
    DeleteFiles(fOutDir, '*.ini');

    // Initialise file count
    fFileNumber := 0;

    // Record path to master meta data file.
    fMasterIniPath := MakePath(cMasterFileName);

  except
    HandleException(ExceptObject);
  end;
end;

function TIniDataWriter.MakeCatIniName(const ACatID: string): string;
begin
  Result := ACatID + '.ini';
end;

function TIniDataWriter.MakeCatIniPath(const ACatID: string): string;
begin
  Result := MakePath(MakeCatIniName(ACatID));
end;

function TIniDataWriter.MakePath(const AFileName: string): string;
begin
  Result := TPath.Combine(fOutDir, AFileName);
end;

procedure TIniDataWriter.WriteCatProps(const CatID: string;
  const Props: TCategoryData);
var
  Master: TUTF8IniFile;
begin
  // Add entry to master ini file
  Master := fCache.GetIniFile(fMasterIniPath);
  Master.WriteString(CatId, cMasterDescName, Props.Desc);
  Master.WriteString(CatId, cMasterIniName, MakeCatIniName(CatID));
end;

procedure TIniDataWriter.WriteCatSnippets(const CatID: string;
  const SnipList: IStringList);
begin
  // Do nothing
end;

procedure TIniDataWriter.WriteMetaData(const AMetaData: TMetaData);
var
  VersionStr: string;
  KVPairs: TStringList;
  LicenseInfo: IStringList;
begin
  VersionStr := Format(
    '%0:d.%1:d.%2:d',
    [AMetaData.Version.V1, AMetaData.Version.V2, AMetaData.Version.V3]
  );
  KVPairs := TStringList.Create;
  try
    KVPairs.Values[LicenseInfoLicenseNameKey] := AMetaData.LicenseInfo.Name;
    KVPairs.Values[LicenseInfoLicenseSPDXKey] := AMetaData.LicenseInfo.SPDX;
    KVPairs.Values[LicenseInfoLicenseURLKey] := AMetaData.LicenseInfo.URL;
    KVPairs.Values[LicenseInfoCopyrightDateKey] := AMetaData.CopyrightInfo.Date;
    KVPairs.Values[LicenseInfoCopyrightHolderKey] :=
      AMetaData.CopyrightInfo.Holder;
    KVPairs.Values[LicenseInfoCopyrightHolderURLKey] :=
      AMetaData.CopyrightInfo.HolderURL;
    LicenseInfo := TIStringList.Create(KVPairs);
  finally
    KVPairs.Free;
  end;

  WriteTextFile(VersionFileName, VersionStr);
  WriteTextFile(LicenseFileName, AMetaData.LicenseInfo.Text);
  WriteTextFile(LicenseInfoFileName, LicenseInfo);
  WriteTextFile(ContributorsFileName, AMetaData.CopyrightInfo.Contributors);
  WriteTextFile(AcknowledgementsFileName, AMetaData.Acknowledgements);
end;

procedure TIniDataWriter.WriteSnippetDepends(const SnippetKey: string;
  const Depends: IStringList);
begin
  fCurrentCatIni.WriteString(
    SnippetKey, cDependsName, Depends.GetText(',', False)
  );
end;

procedure TIniDataWriter.WriteSnippetProps(const SnippetKey: string;
  const Props: TSnippetData);
const
  Kinds: array[TSnippetKind] of string = (
    'freeform',     // skFreeform
    'routine',      // skRoutine
    'const',        // skConstant
    'type',         // skTypeDef
    'unit',         // skUnit
    'class'         // skClass
  );
  CompileResults: array[TCompileResult] of Char = (
    'Y',    // crSuccess
    'Y',    // crWarning
    'N',    // crError
    'Q'     // crQuery
  );
  TestInfo: array[TSnippetTestInfo] of string = (
    {TODO -cVault: New empty entries added because new entries have been added
            to TSnippetTestInfo. Need to update code to support new Advanced.XXX
            entries in DCSC v2.2}
    'none',       // stiNone
    'basic',      // stiBasic
    'advanced',   // stiAdvanced
    '',           // stiUnitTests
    ''            // stiDemoCode
  );
var
  SourceFileName: string;
  SourceFilePath: string;
  CompilerID: TCompilerID;
  CompileResult: Char;
begin

  fCurrentCatIni := fCache.GetIniFile(MakeCatIniPath(Props.Cat));

  try
    // Write source code file
    Inc(fFileNumber);
    SourceFileName := IntToStr(fFileNumber) + '.dat';
    SourceFilePath := MakePath(SourceFileName);
    TFileIO.WriteAllText(SourceFilePath, Props.SourceCode, TEncoding.UTF8, True);

    // snippet kind
    fCurrentCatIni.WriteString(SnippetKey, cKindName, Kinds[Props.Kind]);

    // display name, if set
    if (Props.DisplayName <> '') then
      {TODO -cVault: strictly, for v2 format, this name must be <=64 chars}
      fCurrentCatIni.WriteString(SnippetKey, cDisplayName, Props.DisplayName);

    // description (must be set for v2)
    fCurrentCatIni.WriteString(
      SnippetKey,
      cDescExName,
      DOUBLEQUOTE + ActiveTextToREML(Props.Desc) + DOUBLEQUOTE
    );

    // extra info, if set
    if Props.Extra.HasContent then
      fCurrentCatIni.WriteString(
        SnippetKey,
        cExtraName,
        DOUBLEQUOTE + ActiveTextToREML(Props.Extra) + DOUBLEQUOTE
      );

    // snippet file reference
    fCurrentCatIni.WriteString(SnippetKey, cSnipFileName, SourceFileName);

    // compiler info
    for CompilerID := Low(TCompilerID) to High(TCompilerID) do
    begin
      CompileResult := CompileResults[Props.CompilerResults[CompilerID]];
      if CompileResult <> 'Q' then
          fCurrentCatIni.WriteString(
            SnippetKey, cCompilerIDNames[CompilerID], CompileResult
          );
    end;

    // test info: only write if not basic
    {TODO -cVault: Add support for AdvancedTest .Level & .URL}
    if Props.TestInfo <> stiBasic then
      fCurrentCatIni.WriteString(
        SnippetKey, cTestInfoName, TestInfo[Props.TestInfo]
      );

  except

    HandleException(ExceptObject);

  end;
  // NOTE:
  //   The following deprecated keys are not written: alternatives are always
  //   used:
  //   * StandardFormat - Kind is used instead
  //   * Credits        - Extra is used instead
  //   * Credits_URL    - Extra is used instead
  //   * Comments       - Extra is used instead
  //   * Desc           - DescEx is used instead

end;

procedure TIniDataWriter.WriteSnippetUnits(const SnippetKey: string;
  const Units: IStringList);
begin
  fCurrentCatIni.WriteString(
    SnippetKey, cUnitsName, Units.GetText(',', False)
  );
end;

procedure TIniDataWriter.WriteSnippetXRefs(const SnippetKey: string;
  const XRefs: IStringList);
begin
  fCurrentCatIni.WriteString(
    SnippetKey, cXRefName, XRefs.GetText(',', False)
  );
end;

procedure TIniDataWriter.WriteTextFile(const AFileName, AText: string);
begin
  TFileIO.WriteAllText(MakePath(AFileName), AText, TEncoding.UTF8, True);
end;

procedure TIniDataWriter.WriteTextFile(const AFileName: string;
  const ALines: IStringList);
var
  Content: string;
begin
  Content := ALines.GetText(EOL, False);
  if not StrIsEmpty(Content) then
    Content := Content + EOL;
  WriteTextFile(AFileName, Content);
end;

{ TIniDataWriter.TUTF8IniFile }

procedure TIniDataWriter.TUTF8IniFile.Save;
var
  Data: TStringList;
begin
  Data := TStringList.Create;
  try
    GetStrings(Data);
    TFileIO.WriteAllLines(FileName, Data.ToStringArray, Encoding, True);
  finally
    Data.Free;
  end;
end;

{ TIniDataWriter.TUTF8IniFileCache }

procedure TIniDataWriter.TUTF8IniFileCache.AddIniFile(
  const APathToFile: string);
begin
  if not fCache.ContainsKey(APathToFile) then
    InternalAddIniFile(APathToFile);
end;

constructor TIniDataWriter.TUTF8IniFileCache.Create;
begin
  inherited Create;
  // fCache owns and frees the ini file objects
  fCache := TIniFileMap.Create(
    [doOwnsValues], TTextEqualityComparer.Create
  );
end;

destructor TIniDataWriter.TUTF8IniFileCache.Destroy;
begin
  fCache.Free;  // frees all owned ini file objects in .Values[]
  inherited;
end;

function TIniDataWriter.TUTF8IniFileCache.GetEnumerator: 
  TObjectDictionary<string, TUTF8IniFile>.TPairEnumerator;
begin
  Result := fCache.GetEnumerator;
end;

function TIniDataWriter.TUTF8IniFileCache.GetIniFile(
  const APathToFile: string): TUTF8IniFile;
begin
  if not fCache.ContainsKey(APathToFile) then
    Result := InternalAddIniFile(APathToFile)
  else
    Result := fCache[APathToFile];
end;

function TIniDataWriter.TUTF8IniFileCache.InternalAddIniFile(
  const APathToFile: string): TUTF8IniFile;
begin
  Result := TUTF8IniFile.Create(APathToFile, TEncoding.UTF8);
  fCache.Add(APathToFile, Result);
end;

end.

