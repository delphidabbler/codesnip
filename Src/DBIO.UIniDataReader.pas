{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements code that reads the main CodeSnip database from .ini and .dat
 * files.
}


unit DBIO.UIniDataReader;


interface


uses
  // Delphi
  Classes, Generics.Collections, IniFiles,
  // Project
  DB.UCategory, DB.USnippet, DBIO.UFileIOIntf, UIStringList, UMainDBFileReader;


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
          ///  <summary>Loads database files using correct encoding.</summary>
          fFileReader: TMainDBFileReader;
      public
        ///  <summary>Object constructor. Sets up empty cache.</summary>
        constructor Create(const FileReader: TMainDBFileReader);
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
      ///  <summary>Reads DB files using correct encoding.</summary>
      fFileReader: TMainDBFileReader;
    ///  <summary>
    ///  Returns fully specified name of database master file.
    ///  </summary>
    function MasterFileName: string;
    ///  <summary>
    ///  Returns ID of category associated with a snippet.
    ///  </summary>
    ///  <param name="Snippet">string [in] Name of snippet.</param>
    ///  <returns>string containing category ID</returns>
    function SnippetToCat(const Snippet: string): string;
    ///  <summary>
    ///  Returns name of ini file containing details of a category.
    ///  </summary>
    ///  <param name="CatID">string [in] Id of category.</param>
    ///  <returns>string containing bame of category's ini file</returns>
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
    ///  <summary>
    ///  Returns name of directory where the database is stored.
    ///  </summary>
    function DataDir: string;
    ///  <summary>
    ///  Returns fully specified path to given file name.
    ///  </summary>
    function DataFile(const FileName: string): string;
    ///  <summary>
    ///  Gets a list from ini file of all of items of a specified kind that are
    ///  referenced by a snippet.
    ///  </summary>
    ///  <param name="Snippet">string [in] Name of snippet.</param>
    ///  <param name="RefName">string [in] Name of a key in ini file storing
    ///  comma separated list of references.</param>
    ///  <returns>IStringList containing names of referenced items.</returns>
    function GetSnippetReferences(const Snippet, RefName: string): IStringList;
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
    ///  Gets names of all snippets in a category.
    ///  </summary>
    ///  <param name="CatID">string [in] Id of category.</param>
    ///  <returns>IStringList containing names of snippets.</returns>
    function GetCatSnippets(const CatID: string): IStringList;
    ///  <summary>
    ///  Gets properties of a snippet.
    ///  </summary>
    ///  <param name="Snippet">string [in] Name of snippet.</param>
    ///  <param name="Props">TSnippetData [in/out] Receives empty property
    ///  record and updates relevant property fields.</param>
    procedure GetSnippetProps(const Snippet: string; var Props: TSnippetData);
    ///  <summary>
    ///  Gets list of all snippets that are cross referenced by a specified
    ///  snippet.
    ///  </summary>
    ///  <param name="Snippet">string [in] Name of snippet.</param>
    ///  <returns>IStringList containing snippet names.</returns>
    function GetSnippetXRefs(const Snippet: string): IStringList;
    ///  <summary>
    ///  Gets list of all snippets on which a specified snippet depends.
    ///  </summary>
    ///  <param name="Snippet">string [in] Name of snippet.</param>
    ///  <returns>IStringList containing snippet names.</returns>
    function GetSnippetDepends(const Snippet: string): IStringList;
    ///  <summary>
    ///  Gets list of all units referenced by a snippet.
    ///  </summary>
    ///  <param name="Snippet">string [in] Name of snippet.</param>
    ///  <returns>IStringList containing unit names.</returns>
    function GetSnippetUnits(const Snippet: string): IStringList;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  ActiveText.UMain, Compilers.UGlobals, DB.USnippetKind, UComparers, UConsts,
  UIniDataLoader, USnippetExtraHelper, UStrUtils, UUtils;


const
  // Name of master file that defines database
  cMasterFileName = 'categories.ini';
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
begin
  inherited Create;
  fDBDir := DBDir;
  // Create helper objects used to speed up access to ini files
  if DatabaseExists then
  begin
    fFileReader := TMainDBFileReader.Create(MasterFileName);
    fIniCache := TIniFileCache.Create(fFileReader);
    try
      fMasterIni := TDatabaseIniFile.Create(fFileReader, MasterFileName);
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

destructor TIniDataReader.Destroy;
begin
  fFileReader.Free;
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
  CatIni: TCustomIniFile; // accesses .ini file associated with category
  SnipList: TStringList;  // list of snippets in category
begin
  try
    // Snippet names are names of sections in category's .ini file
    CatIni := fIniCache.GetIniFile(CatToCatIni(CatID));
    SnipList := TStringList.Create;
    try
      CatIni.ReadSections(SnipList);
      Result := TIStringList.Create(SnipList);
    finally
      SnipList.Free;
    end;
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetSnippetDepends(const Snippet: string): IStringList;
begin
  Result := GetSnippetReferences(Snippet, cDependsName);
end;

procedure TIniDataReader.GetSnippetProps(const Snippet: string;
  var Props: TSnippetData);
var
  CatIni: TCustomIniFile; // .ini file associated with snippet's category
  CatID: string;          // snippet's category id

  // ---------------------------------------------------------------------------
  /// <summary>Reads "StandardFormat" value from ini file.</summary>
  function GetStdFormatProperty: Boolean;
  begin
    Result := CatIni.ReadBool(Snippet, cStdFormatName, True);
  end;

  ///  <summary>Reads "Kind" value from ini file.</summary>
  function GetKindProperty: TSnippetKind;
  var
    KindStr: string;  // string value read from ini file
  begin
    KindStr := CatIni.ReadString(Snippet, cKindName, '');
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
      Extra := CatIni.ReadString(Snippet, cExtraName, '');
      if Extra <> '' then
        // There is an "extra" value: use it to set Extra property. We ignore
        // any credits, credits url and comments values in this case
        Result := TSnippetExtraHelper.BuildActiveText(Extra)
      else
        // There is no "extra" value: use any comments, credits and credits URL
        // values to set Extra property
        Result := TSnippetExtraHelper.BuildActiveText(
          CatIni.ReadString(Snippet, cCommentsName, ''),
          CatIni.ReadString(Snippet, cCreditsName, ''),
          CatIni.ReadString(Snippet, cCreditsURLName, '')
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
    SnipFileName := CatIni.ReadString(Snippet, cSnipFileName, '');
    try
      Result := fFileReader.ReadAllText(DataFile(SnipFileName));
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
      CompRes := CatIni.ReadString(Snippet, cCompilerIDNames[CompID], '?');
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
    REML := CatIni.ReadString(Snippet, cDescExName, '');
    if REML <> '' then
      Result := TSnippetExtraHelper.BuildActiveText(REML)
    else
    begin
      PlainText := CatIni.ReadString(Snippet, cDescName, '');
      if PlainText <> '' then
        Result := TSnippetExtraHelper.PlainTextToActiveText(PlainText)
      else
        Result := TActiveTextFactory.CreateActiveText;
    end;
  end;

  ///  <summary>Gets snippet's display name from ini file.</summary>
  function GetDisplayNameProperty: string;
  begin
    Result := CatIni.ReadString(Snippet, cDisplayName, '');
  end;

  ///  <summary>Get's snippet's test info from ini file.</summary>
  function GetTestInfoProperty: TSnippetTestInfo;
  var
    Str: string;  // string value read from ini file
  begin
    Str := CatIni.ReadString(Snippet, cTestInfoName, 'basic');
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
    CatID := SnippetToCat(Snippet);
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

function TIniDataReader.GetSnippetReferences(const Snippet,
  RefName: string): IStringList;
var
  CatIni: TCustomIniFile; // accesses snippet's category's .ini
begin
  try
    // References are contained in comma separated value in category's ini file
    // under snippet's section
    CatIni := fIniCache.GetIniFile(CatToCatIni(SnippetToCat(Snippet)));
    Result := CommaStrToStrings(CatIni.ReadString(Snippet, RefName, ''));
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetSnippetUnits(const Snippet: string): IStringList;
begin
  Result := GetSnippetReferences(Snippet, cUnitsName);
end;

function TIniDataReader.GetSnippetXRefs(const Snippet: string): IStringList;
begin
  Result := GetSnippetReferences(Snippet, cXRefName);
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
  SnippetName: string;        // each snippet name in a category
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
    for SnippetName in CatSnippets do
      fSnippetCatMap.Add(SnippetName, CatIdx);
  end;
end;

function TIniDataReader.MasterFileName: string;
begin
  Result := DataFile(cMasterFileName);
end;

function TIniDataReader.SnippetToCat(const Snippet: string): string;
var
  CatIdx: Integer;  // index of category in category list for this snippet
resourcestring
  // Error message
  sMissingSnippet = 'Snippet "%s" not found in database.';
begin
  if not fSnippetCatMap.ContainsKey(Snippet) then
    raise EDataIO.CreateFmt(sMissingSnippet, [Snippet]);
  CatIdx := fSnippetCatMap[Snippet];
  Result := fCatIDs[CatIdx];
end;

{ TIniDataReader.TIniFileCache }

constructor TIniDataReader.TIniFileCache.Create(
  const FileReader: TMainDBFileReader);
begin
  inherited Create;
  fFileReader := FileReader;
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
    fCache.Add(PathToFile, TDatabaseIniFile.Create(fFileReader, PathToFile));
  Result := fCache[PathToFile];
end;

end.

