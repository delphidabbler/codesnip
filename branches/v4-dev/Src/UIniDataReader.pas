{
 * UIniDataReader.pas
 *
 * Implements code that reads code snip database from .ini and .dat files.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UIniDataReader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UIniDataReader;


interface


uses
  // Delphi
  Classes, Generics.Collections, IniFiles,
  // Project
  UIStringList, UMainDBFileReader, USnipData, USnippets;


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
      fCatNames: TStringList;
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
    ///  <param name="Routine">string [in] Name of snippet.</param>
    ///  <returns>string containing category ID</returns>
    function RoutineToCat(const Routine: string): string;
    ///  <summary>
    ///  Returns name of ini file containing details of a category.
    ///  </summary>
    ///  <param name="Cat">string [in] ID of category.</param>
    ///  <returns>string containing bame of category's ini file</returns>
    function CatToCatIni(const Cat: string): string;
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
    ///  <param name="Routine">string [in] Name of snippet.</param>
    ///  <param name="RefName">string [in] Name of a key in ini file storing
    ///  comma separated list of references.</param>
    ///  <returns>IStringList containing names of referenced items.</returns>
    function GetRoutineReferences(const Routine, RefName: string): IStringList;
  protected // do not make strict
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
    function GetAllCatNames: IStringList;
    ///  <summary>
    ///  Gets properties of a category.
    ///  </summary>
    ///  <param name="Cat">string [in] ID of category.</param>
    ///  <param name="Props">TCategoryData [in/out] Receives empty property
    ///  record and updates relevant property fields.</param>
    procedure GetCatProps(const Cat: string; var Props: TCategoryData);
    ///  <summary>
    ///  Gets names of all snippets in a category.
    ///  </summary>
    ///  <param name="Cat">string [in] ID of category.</param>
    ///  <returns>IStringList containing names of snippets.</returns>
    function GetCatRoutines(const Cat: string): IStringList;
    ///  <summary>
    ///  Gets propertyies of a snippet.
    ///  </summary>
    ///  <param name="Routine">string [in] Name of snippet.</param>
    ///  <param name="Props">TSnippetData [in/out] Receives empty property
    ///  record and updates relevant property fields.</param>
    procedure GetRoutineProps(const Routine: string; var Props: TSnippetData);
    ///  <summary>
    ///  Gets list of all snippets that are cross referenced by a specified
    ///  snippet.
    ///  </summary>
    ///  <param name="Routine">string [in] Name of snippet.</param>
    ///  <returns>IStringList containing snippet names.</returns>
    function GetRoutineXRefs(const Routine: string): IStringList;
    ///  <summary>
    ///  Gets list of all snippets on which a specified snippet depends.
    ///  </summary>
    ///  <param name="Routine">string [in] Name of snippet.</param>
    ///  <returns>IStringList containing snippet names.</returns>
    function GetRoutineDepends(const Routine: string): IStringList;
    ///  <summary>
    ///  Gets list of all units referenced by a snippet.
    ///  </summary>
    ///  <param name="Routine">string [in] Name of snippet.</param>
    ///  <returns>IStringList containing unit names.</returns>
    function GetRoutineUnits(const Routine: string): IStringList;
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
  end;


{*******************************************************************************
*                                                                              *
* This unit provides a database reader that gets code snippet data from .ini   *
* and .dat files. The object implements the IDataReader interface to provide   *
* access to the data when creating the code snippets global object. The data   *
* files are described below:                                                   *
*                                                                              *
* ---------------------------------------------------------------------------- *
* categories.ini                                                               *
*   This is the master ini file. It has one section for each category in the   *
*   database. The section names are the ids of the categories. Each category   *
*   section contains two values:                                               *
*   1)  Desc - a description of the category.                                  *
*   2)  Ini - the name of a further .ini file that provides details of all the *
*       snippets in the category.                                              *
* ---------------------------------------------------------------------------- *
* <category>.ini                                                               *
*   There is one of these files for each category - named after the Ini        *
*   entries in categories.ini. The file contains details of all the snippets   *
*   in the category. The section names are the names of the snippets. Each     *
*   snippet section contains the following values (values shown in [ ] are     *
*   optional):                                                                 *
*   1)  Desc - a description of the snippet.                                   *
*   2)  Units - comma separated list of units used by the snippet.             *
*   3)  Depends - comma separated list of other snippets in database required  *
*       by the snippet.                                                        *
*   4)  Snip - name of a .dat file that contains the snippet source code.      *
*   5)  [SeeAlso] - comma separated list of snippets in database that are      *
*       cross referenced from this snippet.                                    *
*   6)  [Credits] - text that gives credit to another individual. May contain  *
*       text text of a hyperlink enclosed in [ ]. If present the URL is in the *
*       Credits_URL value. Ignored if Extra is specified.                      *
*   7)  [Credits_URL] - URL of any hyperlink in Credits. Ignored if Extra is   *
*       specified.                                                             *
*   8)  [Comments] - further comments pertaining to snippet. Ignored if Extra  *
*       is specified.                                                          *
*   9)  [Extra] - extra information pertaining to snippet. If present then     *
*       Credits, Credits_URL and Comments are ignored. Contains text in REML,  *
*       "Routine Extra Markup Language".                                       *
*   10) DelphiX (where X is 2..7)                                              *
*         or                                                                   *
*       DelphiXXXXWin32 (where XXXX is 2005, 2006 or 2009)                     *
*         or                                                                   *
*       DelphiXXXX (where XXXX is 2007 or 2010)                                *
*         or                                                                   *
*       DelphiXE                                                               *
*         or                                                                   *
*       FPC (Free Pascal) - one entry for each supported compiler that         *
*       indicates compatibility of the snippet with given compiler. Values are *
*       one of: Y - compiles without warning; N - does not compile; Q or ? -   *
*       not tested; W - compiles with warnings.                                *
*   11) [StandardFormat] - whether snippet is in standard format. Assumes true *
*       if not present and there is no Kind value.                             *
*   12) [Kind] - kind of snippet. One of freeform, snippet, type or const.     *
*       If value not present then snippet is assumed unless StandardFormat is  *
*       present and 0, when freeform is assumed.                               *
*                                                                              *
* ---------------------------------------------------------------------------- *
* <999>.dat                                                                    *
*   There is one of these files for each snippet in the database. It stores    *
*   the snippet's Pascal source code. The name of the file is specified by the *
*   Snip value in the <category>.ini file.                                     *
* ---------------------------------------------------------------------------- *
*                                                                              *
* The database reader object interprets these files and provides the common    *
* view onto them expected by the program. See the comments in USnipData.pas    *
* for details.                                                                 *
*                                                                              *
*******************************************************************************}


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Compilers.UGlobals, UActiveText, UComparers, UConsts, UIniDataLoader,
  URoutineExtraHelper, UUtils;


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
  cExtraName = 'Extra';               // extra information for snippet
  cCreditsName = 'Credits';           // snippet credits
  cCreditsURLName = 'Credits_URL';    // url relating to snippet credits
  cCommentsName = 'Comments';         // snippet additional comments
  cDescName = 'Desc';                 // snippet description
  cSnipFileName = 'Snip';             // name of snippet's snippet file
  cStdFormatName = 'StandardFormat';  // whether snippet in std format
  cKindName = 'Kind';                 // kind of snippet
  cCompilerIDNames:                   // snippet's compiler results for each
    array[TCompilerID] of string = (
    'Delphi2', 'Delphi3', 'Delphi4', 'Delphi5', 'Delphi6', 'Delphi7',
    'Delphi2005Win32', 'Delphi2006Win32', 'Delphi2007', 'Delphi2009Win32',
    'Delphi2010', 'DelphiXE',
    'FPC'
  );

{ TIniDataReader }

function TIniDataReader.CatToCatIni(const Cat: string): string;
begin
  Result := DataFile(fMasterIni.ReadString(Cat, cMasterIniName, ''));
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
      fCatNames := TStringList.Create;
      fSnippetCatMap := TSnippetCatMap.Create(TSameTextEqualityComparer.Create);
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
  fCatNames.Free;
  fMasterIni.Free;
  inherited;
end;

function TIniDataReader.GetAllCatNames: IStringList;
begin
  Result := TIStringList.Create(fCatNames);
end;

procedure TIniDataReader.GetCatProps(const Cat: string;
  var Props: TCategoryData);
begin
  try
    Props.Desc := fMasterIni.ReadString(Cat, cMasterDescName, '');
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetCatRoutines(const Cat: string): IStringList;
var
  CatIni: TCustomIniFile; // accesses .ini file associated with category
  Routines: TStringList;  // list of snippets in category
begin
  try
    // Snippet names are names of sections in category's .ini file
    CatIni := fIniCache.GetIniFile(CatToCatIni(Cat));
    Routines := TStringList.Create;
    try
      CatIni.ReadSections(Routines);
      Result := TIStringList.Create(Routines);
    finally
      Routines.Free;
    end;
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetRoutineDepends(const Routine: string): IStringList;
begin
  Result := GetRoutineReferences(Routine, cDependsName);
end;

procedure TIniDataReader.GetRoutineProps(const Routine: string;
  var Props: TSnippetData);
var
  CatIni: TCustomIniFile; // .ini file associated with snippet's category
  Cat: string;            // snippet's category

  // ---------------------------------------------------------------------------
  /// <summary>Reads "StandardFormat" value from ini file.</summary>
  function GetStdFormatProperty: Boolean;
  begin
    Result := CatIni.ReadBool(Routine, cStdFormatName, True);
  end;

  ///  <summary>Reads "Kind" value from ini file.</summary>
  function GetKindProperty: TSnippetKind;
  var
    KindStr: string;  // string value read from ini file
  begin
    KindStr := CatIni.ReadString(Routine, cKindName, '');
    if AnsiSameText(KindStr, 'freeform') then
      Result := skFreeform
    else if AnsiSameText(KindStr, 'routine') then
      Result := skRoutine
    else if AnsiSameText(KindStr, 'const') then
      Result := skConstant
    else if AnsiSameText(KindStr, 'type') then
      Result := skTypeDef
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
      Extra := CatIni.ReadString(Routine, cExtraName, '');
      if Extra <> '' then
        // There is an "extra" value: use it to set Extra property. We ignore
        // any credits, credits url and comments values in this case
        Result := TRoutineExtraHelper.BuildActiveText(Extra)
      else
        // There is no "extra" value: use any comments, credits and credits URL
        // values to set Extra property
        Result := TRoutineExtraHelper.BuildActiveText(
          CatIni.ReadString(Routine, cCommentsName, ''),
          CatIni.ReadString(Routine, cCreditsName, ''),
          CatIni.ReadString(Routine, cCreditsURLName, '')
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
    SnipFileName := CatIni.ReadString(Routine, cSnipFileName, '');
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
      CompRes := CatIni.ReadString(Routine, cCompilerIDNames[CompID], '?');
      if CompRes = '' then
        CompRes := '?';
      case CompRes[1] of
        'Y': Result[CompID] := crSuccess;
        'W': Result[CompID] := crWarning;
        'N': Result[CompID] := crError;
        else Result[CompID] := crQuery;
      end;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  try
    // Get name of category associated with this snippet
    Cat := RoutineToCat(Routine);
    // Get snippet properties from values listed under snippet's section in
    // category's .ini file
    CatIni := fIniCache.GetIniFile(CatToCatIni(Cat));
    Props.Kind := GetKindProperty;
    Props.Cat := Cat;
    Props.Desc := CatIni.ReadString(Routine, cDescName, '');
    Props.Extra := GetExtraProperty;
    Props.SourceCode := GetSourceCodeProperty;
    Props.CompilerResults := GetCompilerResultsProperty;
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetRoutineReferences(const Routine,
  RefName: string): IStringList;
var
  CatIni: TCustomIniFile; // accesses snippet's category's .ini
begin
  try
    // References are contained in comma separated value in category's ini file
    // under snippet's section
    CatIni := fIniCache.GetIniFile(CatToCatIni(RoutineToCat(Routine)));
    Result := CommaStrToStrings(CatIni.ReadString(Routine, RefName, ''));
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetRoutineUnits(const Routine: string): IStringList;
begin
  Result := GetRoutineReferences(Routine, cUnitsName);
end;

function TIniDataReader.GetRoutineXRefs(const Routine: string): IStringList;
begin
  Result := GetRoutineReferences(Routine, cXRefName);
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
  CatRoutines: IStringList;   // list of snippets in a single category
begin
  // Read in list of category names
  fMasterIni.ReadSections(fCatNames);
  // We build map of snippet names to categories by reading snippets in each
  // category and referencing that category's id with the snippet name.
  CatRoutines := TIStringList.Create;
  for CatIdx := 0 to Pred(fCatNames.Count) do
  begin
    // Get list of snippets in category ...
    CatRoutines := GetCatRoutines(fCatNames[CatIdx]);
    for SnippetName in CatRoutines do
      fSnippetCatMap.Add(SnippetName, CatIdx);
  end;
end;

function TIniDataReader.MasterFileName: string;
begin
  Result := DataFile(cMasterFileName);
end;

function TIniDataReader.RoutineToCat(const Routine: string): string;
var
  CatIdx: Integer;  // index of category in category list for this snippet
resourcestring
  // Error message
  sMissingRoutine = 'Snippet "%s" not found in database.';
begin
  if not fSnippetCatMap.ContainsKey(Routine) then
    raise EDataIO.CreateFmt(sMissingRoutine, [Routine]);
  CatIdx := fSnippetCatMap[Routine];
  Result := fCatNames[CatIdx];
end;

{ TIniDataReader.TIniFileCache }

constructor TIniDataReader.TIniFileCache.Create(
  const FileReader: TMainDBFileReader);
begin
  inherited Create;
  fFileReader := FileReader;
  // fCache owns and frees the ini file objects
  fCache := TIniFileMap.Create(
    [doOwnsValues], TSameTextEqualityComparer.Create
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

