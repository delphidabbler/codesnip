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
 * Portions created by the Initial Developer are Copyright (C) 2005-2013 Peter
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
  UIStringList, USnipData, USnippets;


type

  {
  TIniDataReader:
    Reads codesnip data from .ini and .dat files.
  }
  TIniDataReader = class sealed(TInterfacedObject, IDataReader)
  strict private
    type
      {
      TIniFileCache:
        Implements a cache of ini file objects, indexed by ini file name.
      }
      TIniFileCache = class(TObject)
      strict private
        type
          // Class that maps ini file names to related ini file objects
          TIniFileMap = TObjectDictionary<string,TCustomIniFile>;
        var fCache: TIniFileMap;  // Maps file names to related ini file objects
      public
        constructor Create;
          {Constructor. Sets up empty cache.
          }
        destructor Destroy; override;
          {Destructor. Releases cache.
          }
        function GetIniFile(const PathToFile: string): TCustomIniFile;
          {Gets reference to ini file object. Creates it if it doesn't exist.
            @param PathToFile [in] Path to ini file.
            @return Required ini file reference.
          }
      end;
    type
      // Class that maps snippet names to category ids
      TSnippetCatMap = TDictionary<string,Integer>;
    var
      fDBDir: string;                 // Database directory
      fMasterIni: TCustomIniFile;     // Reference to master ini file
      fCatNames: TStringList;         // List of category ids in database
      fSnippetCatMap: TSnippetCatMap; // Map of snippet names to category ids
      fIniCache: TIniFileCache;       // Cache of category ini file objects
    function MasterFileName: string;
      {Gets fully specified name of master file depending on which database is
      being accessed.
        @return Required file name.
      }
    function RoutineToCat(const Routine: string): string;
      {Returns name (id) of category associated with a snippet.
        @param Routine [in] Name of snippet.
        @return ID of snippet's category.
      }
    function CatToCatIni(const Cat: string): string;
      {Returns name of ini file associated with a category name (id).
        @param Cat [in] Category ID.
        @return Name of ini file.
      }
    procedure LoadIndices;
      {Loads "indexes" of all names of categories and snippets in database.
      Having these "indexes" available speeds up several of the key methods.
      }
    procedure HandleCorruptDatabase(const EObj: TObject);
      {Called when a corrupt database is encountered. Deletes all files and
      raises exception.
        @param EObj [in] Reference to exception that caused this method to be
          called.
        @except ECodeSnip always raised.
      }
    function DataDir: string;
      {Gets name of directory storing the database being read. Path varies
      according to which database is being read.
        @return Name of directory.
      }
    function DataFile(const FileName: string): string;
      {Gets full path to a file name. Path depends on which database is being
      read.
        @param FileName [in] File name for which path is required.
        @return Required full path to file.
      }
    function GetRoutineReferences(const Routine, RefName: string): IStringList;
      {Get list of all specified references made by a snippet.
        @param Routine [in] Name of required snippet.
        @param RefName [in] Name of value containing comma separated list of
          references in ini file.
        @return List of names of references.
      }
  protected // do not make strict
    { IDataReader }
    function DatabaseExists: Boolean;
      {Check if the database exists. This method is always called first. No
      other methods are called if this method returns false.
        @return True if database exists, False if not.
      }
    function GetAllCatNames: IStringList;
      {Get names of all categories in database.
        @return List of category names.
      }
    procedure GetCatProps(const Cat: string; var Props: TCategoryData);
      {Get properties of a category.
        @param Cat [in] Name of required category.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of category properties by implementor.
      }
    function GetCatRoutines(const Cat: string): IStringList;
      {Get names of all snippets in a category.
        @param Cat [in] Name of category containing snippets.
        @return List of snippets names.
      }
    procedure GetRoutineProps(const Routine: string; var Props: TSnippetData);
      {Get properties of a snippet. These are the fields of the snippet's
      record in the snippets "table".
        @param Routine [in] Name of required snippet.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of snippet properties by implementor.
      }
    function GetRoutineXRefs(const Routine: string): IStringList;
      {Get list of all snippets that are cross referenced by a snippet.
        @param Routine [in] Name of snippet we need cross references for.
        @return List of snippet names.
      }
    function GetRoutineDepends(const Routine: string): IStringList;
      {Get list of all snippets on which a given snippet depends.
        @param Routine [in] Name of required snippet.
        @return List of snippet names.
      }
    function GetRoutineUnits(const Routine: string): IStringList;
      {Get list of all units referenced by a snippet.
        @param Routine [in] Name of required snippet.
        @return List of unit names.
      }
    class function CommaStrToStrings(const CommaStr: string): IStringList;
      {Extracts command delimited text fields into a string list.
        @param CommaStr [in] String of comma delimited fields.
        @return String list containing fields.
      }
  public
    constructor Create(const DBDir: string);
      {Constructor. Sets up data reader object.
        @param DBDir [in] Directory where database is stored.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
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
*       one of:                                                                *
*         Y - Compiles without warning;                                        *
*         W - Compiles with warnings. This is now treated the same as "Y".     *
*             This is because the user can now configure which warnings are    *
*             displayed, therefore this setting is now meaningless. See        *
*             feature request #3290359.                                        *
*         N - Does not compile;                                                *
*         Q - Not tested;                                                      *
*         ? - Not tested.                                                      *
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
    'Delphi2010', 'DelphiXE', 'DelphiXE2', 'DelphiXE3', 'DelphiXE4',
    'DelphiXE5', 'FPC'
  );

{ TIniDataReader }

function TIniDataReader.CatToCatIni(const Cat: string): string;
  {Returns name of ini file associated with a category name (id).
    @param Cat [in] Category ID.
    @return Name of ini file.
  }
begin
  // Ini files are all in application's data directory and names of .ini files
  // are in master .ini file under category's section, in a value named 'Ini'.
  Result := DataFile(fMasterIni.ReadString(Cat, cMasterIniName, ''));
end;

class function TIniDataReader.CommaStrToStrings(
  const CommaStr: string): IStringList;
  {Extracts command delimited text fields into a string list.
    @param CommaStr [in] String of comma delimited fields.
    @return String list containing fields.
  }
begin
  Result := TIStringList.Create(CommaStr, ',', False, True);
end;

constructor TIniDataReader.Create(const DBDir: string);
  {Constructor. Sets up data reader object.
    @param DBDir [in] Directory where database is stored.
  }
begin
  inherited Create;
  fDBDir := DBDir;
  // Create helper objects used to speed up access to ini files
  if DatabaseExists then
  begin
    fIniCache := TIniFileCache.Create;
    try
      fMasterIni := TDatabaseIniFile.Create(MasterFileName);
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
  {Check if the database exists. This method is always called first. No other
  methods are called if this method returns false.
    @return True if database exists, False if not.
  }
begin
  Result := FileExists(MasterFileName);
end;

function TIniDataReader.DataDir: string;
  {Gets name of directory storing the database being read. Path varies according
  to which database is being read.
    @return Name of directory.
  }
begin
  Result := ExcludeTrailingPathDelimiter(fDBDir)
end;

function TIniDataReader.DataFile(const FileName: string): string;
  {Gets full path to a file name. Path depends on which database is being read.
    @param FileName [in] File name for which path is required.
    @return Required full path to file.
  }
begin
  Result := IncludeTrailingPathDelimiter(DataDir) + FileName;
end;

destructor TIniDataReader.Destroy;
  {Destructor. Tears down object.
  }
begin
  fIniCache.Free;
  fSnippetCatMap.Free;
  fCatNames.Free;
  fMasterIni.Free;
  inherited;
end;

function TIniDataReader.GetAllCatNames: IStringList;
  {Get names of all categories in database.
    @return List of category names.
  }
begin
  Result := TIStringList.Create(fCatNames);
end;

procedure TIniDataReader.GetCatProps(const Cat: string;
  var Props: TCategoryData);
  {Get properties of a category.
    @param Cat [in] Name of required category.
    @param Props [in/out] Empty properties passed in. Record fields set to
      values of category properties by implementor.
  }
begin
  try
    Props.Desc := fMasterIni.ReadString(Cat, cMasterDescName, '');
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TIniDataReader.GetCatRoutines(const Cat: string): IStringList;
  {Get names of all snippets in a category.
    @param Cat [in] Name of category containing snippets.
    @return List of snippets names.
  }
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
  {Get list of all snippets on which a given snippet depends.
    @param Routine [in] Name of required snippet.
    @return List of snippet names.
  }
begin
  Result := GetRoutineReferences(Routine, cDependsName);
end;

procedure TIniDataReader.GetRoutineProps(const Routine: string;
  var Props: TSnippetData);
  {Get properties of a snippet. These are the fields of the snippet's record in
  the snippets "table".
    @param Routine [in] Name of required snippet.
    @param Props [in/out] Empty properties passed in. Record fields set to
      values of snippet properties by implementor.
  }
var
  CatIni: TCustomIniFile; // .ini file associated with snippet's category
  Cat: string;            // snippet's category

  // ---------------------------------------------------------------------------
  function GetStdFormatProperty: Boolean;
    {Reads StandardFormat value from ini file.
      @return Value of property or a default if property not present.
    }
  begin
    Result := CatIni.ReadBool(Routine, cStdFormatName, True);
  end;

  function GetKindProperty: TSnippetKind;
    {Reads Kind property from ini file.
      @return Value of property or a calculated value based on StandardFormat if
        Kind property not present.
    }
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

  function GetExtraProperty: IActiveText;
    {Builds Extra active text object from file data.
      @return Required active text.
    }
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

  function GetSourceCodeProperty: string;
    {Gets source code from file referenced in ini file.
      @return Required source code.
    }
  var
    SnipFileName: string; // name of file containing source code
  begin
    SnipFileName := CatIni.ReadString(Routine, cSnipFileName, '');
    try
      Result := FileToString(DataFile(SnipFileName));
    except
      // if error loading file then database is corrupt
      on E: EFOpenError do
        raise EDataIO.Create(E);
      else
        raise;
    end;
  end;

  function GetCompilerResultsProperty: TCompileResults;
    {Builds compiler results from data in ini file.
      @return Required compiler results.
    }
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
        'W', // warning result now treated as success: see notes above
        'Y': Result[CompID] := crSuccess;
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
  {Get list of all specified references made by a snippet.
    @param Routine [in] Name of required snippet.
    @param RefName [in] Name of value containing comma separated list of
      references in ini file.
    @return List of names of references.
  }
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
  {Get list of all units referenced by a snippet.
    @param Routine [in] Name of required snippet.
    @return List of unit names.
  }
begin
  Result := GetRoutineReferences(Routine, cUnitsName);
end;

function TIniDataReader.GetRoutineXRefs(const Routine: string): IStringList;
  {Get list of all snippets that are cross referenced by a snippet.
    @param Routine [in] Name of snippet we need cross references for.
    @return List of snippet names.
  }
begin
  Result := GetRoutineReferences(Routine, cXRefName);
end;

procedure TIniDataReader.HandleCorruptDatabase(const EObj: TObject);
  {Deletes a corrupt database and raises an exception.
    @except ECodeSnip always raised.
  }
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
  {Loads "indexes" of all names of categories and snippets in database. Having
  these "indexes" available speeds up several of the key methods.
  }
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
  {Gets fully specified name of master file depending on which database is being
  accessed.
    @return Required file name.
  }
begin
  Result := DataFile(cMasterFileName);
end;

function TIniDataReader.RoutineToCat(const Routine: string): string;
  {Returns name (id) of category associated with a snippet.
    @param Routine [in] Name of snippet.
    @return ID of snippet's category.
  }
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

constructor TIniDataReader.TIniFileCache.Create;
  {Constructor. Sets up empty cache.
  }
begin
  inherited Create;
  // fCache owns the ini file objects it stores in .Values[] and frees the
  // objects when they are deleted from .Values[] of when fCache is freed.
  fCache := TIniFileMap.Create(
    [doOwnsValues], TSameTextEqualityComparer.Create
  );
end;

destructor TIniDataReader.TIniFileCache.Destroy;
  {Destructor. Releases cache.
  }
begin
  fCache.Free;  // frees owned .Values[] objects
  inherited;
end;

function TIniDataReader.TIniFileCache.GetIniFile(
  const PathToFile: string): TCustomIniFile;
  {Gets reference to ini file object. Creates it if it doesn't exist.
    @param PathToFile [in] Path to ini file.
    @return Required ini file reference.
  }
begin
  if not fCache.ContainsKey(PathToFile) then
    fCache.Add(PathToFile, TDatabaseIniFile.Create(PathToFile));
  Result := fCache[PathToFile];
end;

end.

