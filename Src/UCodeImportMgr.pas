{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that handles import of a codesnip export file into
 * the user-defined database.
}


unit UCodeImportMgr;


interface


uses
  // Delphi
  Generics.Collections, Generics.Defaults,
  // Project
  UCodeImportExport, UExceptions, UIStringList;


type
  ///  <summary>
  ///  Stores information about if and how a snippet is to be imported.
  ///  </summary>
  TImportInfo = record
  strict private
    // Property values
    fOrigKey: string;
    fImportAsKey: string;
    fSkip: Boolean;
  public
    ///  <summary>Initialises properties to given values.</summary>
    constructor Create(const AOrigKey, AImportAsKey: string;
      const ASkip: Boolean = False);
    ///  <summary>Snippet key per import file.</summary>
    property OrigKey: string read fOrigKey;
    ///  <summary>Key of snippet to be used when updating database.</summary>
    ///  <remarks>Can be changed by user.</remarks>
    property ImportAsKey: string read fImportAsKey write fImportAsKey;
    ///  <summary>Flag indicating if snippet is to be skipped (ignored) when
    ///  updating database.</summary>
    property Skip: Boolean read fSkip write fSkip;
  end;

type
  ///  <summary>
  ///  Comparer for TImportInfo objects.
  ///  </summary>
  TImportInfoComparer = class(TComparer<TImportInfo>, IComparer<TImportInfo>)
  public
    ///  <summary>Compares Left and Right TImportInfo records. Returns -ve if
    ///  Left less than Right, 0 if equal or +ve if Left greater than
    ///  Right.</summary>
    function Compare(const Left, Right: TImportInfo): Integer; override;
  end;

type
  ///  <summary>
  ///  List of TImportInfo records. Designed to store descriptions of how to
  ///  import each snippet in an import file.
  ///  </summary>
  TImportInfoList = class(TList<TImportInfo>)
  public
    ///  <summary>Constructs list with appropriate comparer.</summary>
    constructor Create;
    ///  <summary>Finds a record based on its OrigKey field value.</summary>
    ///  <param name="Key">string [in] Key to be found.</param>
    ///  <param name="ImportInfo">TImportInfo [out] Found record. Undefined if
    ///  Name not found.</param>
    ///  <returns>Boolean: True if Key found, False if not.</returns>
    function FindByKey(const Key: string; out ImportInfo: TImportInfo):
      Boolean;
    ///  <summary>Returns index of record in list whose OrigKey field matches
    ///  given name or -1 if name not found.</summary>
    function IndexOfKey(const Key: string): Integer;
  end;

type
  ///  <summary>
  ///  Manages import of a codesnip export file into the user-defined database.
  ///  </summary>
  ///  <remarks>
  ///  Designed for ease of interaction with a suitable UI.
  ///  </remarks>
  TCodeImportMgr = class sealed(TObject)
  strict private
    var
      ///  <summary>List of snippet information read from import file.</summary>
      fSnippetInfoList: TSnippetInfoList;
      ///  <summary>Value of ImportInfo property.</summary>
      fImportInfoList: TImportInfoList;
    ///  <summary>Initialises import information list with details of snippets
    ///  read from import file.</summary>
    procedure InitImportInfoList;
    ///  <summary>Returns list of names that can't be used to rename an imported
    ///  snippet.</summary>
    ///  <param name="ExcludedName">string [in] Name of snippet to be excluded
    ///  from import list.</param>
    ///  <returns>IStringList: List of disallowed snippet names.</returns>
    ///  <remarks>List is made up of all names of snippets in user database plus
    ///  names of all imported snippets except for ExcludedName. ExcludedName
    ///  should be the name of a snippet being renamed.</remarks>
    function DisallowedNames(const ExcludedName: string): IStringList;
    ///  <summary>Returns a name for snippet SnippetName that does not already
    ///  exist in user database or imported snippet list.</summary>
    ///  <remarks>
    ///  <para>If SnippetName is not in user database then it is returned
    ///  unchanged.</para>
    ///  <para>If SnippetName is in user database then numbers are appended
    ///  sequentially until a unique name is found.</para>
    ///  </remarks>
    function GetUniqueSnippetName(const SnippetName: string): string;
  public
    ///  <summary>Constructor. Sets up object.</summary>
    constructor Create;
    ///  <summary>Destructor. Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Imports snippet info from file without updating database.
    ///  </summary>
    ///  <remarks>ImportInfo list property is initialised ready for
    ///  customisation.</remarks>
    procedure Import(const FileName: string);
    ///  <summary>Updates database based on imported snippets and customisation
    ///  described by ImportInfo property.</summary>
    ///  <remarks>Any existing snippets with same name as imported snippets are
    ///  overwritten.</remarks>
    procedure UpdateDatabase;
    ///  <summary>List of information describing if and how to import snippets
    ///  in import file. Permits customisation of import.</summary>
    property ImportInfo: TImportInfoList read fImportInfoList;
  end;

type
  ///  <summary>
  ///  Class of exception raised when import manager encounters an anticipated
  ///  error.
  ///  </summary>
  ECodeImportMgr = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  Classes,
  // Project
  ActiveText.UMain,
  DB.UCollections,
  DB.UMain,
  DB.USnippet,
  UIOUtils,
  USnippetIDs,
  UStrUtils;


{ TCodeImportMgr }

constructor TCodeImportMgr.Create;
begin
  inherited Create;
  SetLength(fSnippetInfoList, 0);
  fImportInfoList := TImportInfoList.Create;
end;

destructor TCodeImportMgr.Destroy;
begin
  fImportInfoList.Free;
  SetLength(fSnippetInfoList, 0);
  inherited;
end;

function TCodeImportMgr.DisallowedNames(const ExcludedName: string):
  IStringList;
var
  Snippet: TSnippet;          // each snippet in user database
  SnippetInfo: TSnippetInfo;  // info about each imported snippet
begin
  Result := TIStringList.Create;
  Result.CaseSensitive := False;
  for Snippet in Database.Snippets do
    if Snippet.CollectionID <> TCollectionID.__TMP__MainDBCollectionID then
      Result.Add(Snippet.Key);
  for SnippetInfo in fSnippetInfoList do
    if not StrSameText(SnippetInfo.Key, ExcludedName) then
      Result.Add(SnippetInfo.Key);
end;

function TCodeImportMgr.GetUniqueSnippetName(
  const SnippetName: string): string;
var
  UsedNames: IStringList; // list of snippet names in use
  Postfix: Cardinal;      // number to be appended to name to make unique
begin
  UsedNames := DisallowedNames(SnippetName);
  if not UsedNames.Contains(SnippetName) then
    Exit(SnippetName);
  Postfix := 1;
  repeat
    Inc(PostFix);
    Result := SnippetName + IntToStr(PostFix);
  until not UsedNames.Contains(Result);
end;

procedure TCodeImportMgr.Import(const FileName: string);
var
  Data: TBytes; // content of import file as bytes
begin
  fImportInfoList.Clear;
  try
    Data := TFileIO.ReadAllBytes(FileName);
    TCodeImporter.ImportData(fSnippetInfoList, Data);
  except
    on E: EStreamError do
      raise ECodeImportMgr.Create(E);
    on E: ECodeImporter do
      raise ECodeImportMgr.Create(E);
  end;
  InitImportInfoList;
end;

procedure TCodeImportMgr.InitImportInfoList;
var
  SnippetInfo: TSnippetInfo;  // info about each snippet in import file
begin
  fImportInfoList.Clear;
  for SnippetInfo in fSnippetInfoList do
  begin
    fImportInfoList.Add(
      TImportInfo.Create(
        SnippetInfo.Key, GetUniqueSnippetName(SnippetInfo.Key)
      )
    );
  end;
end;

procedure TCodeImportMgr.UpdateDatabase;

  // Adjusts a snippet's dependency list so that main database is searched for a
  // required snippet if it is not in the user database.
  procedure AdjustDependsList(const Depends: ISnippetIDList);
  var
    Idx: Integer;           // loops through dependencies
    SnippetID: TSnippetID;  // each snippet ID in dependency list
    CollectionID: TCollectionID;
  begin
    // NOTE: The data file format does not record which database a required
    // snippet belongs to, so we first look in the user database and if it's
    // not there, we assume the main database
    for Idx := 0 to Pred(Depends.Count) do
    begin
      SnippetID := Depends[Idx];
      CollectionID := TCollectionID.__TMP__UserDBCollectionID;
      if Database.Snippets.Find(SnippetID.Key, CollectionID) = nil then
        CollectionID := TCollectionID.__TMP__MainDBCollectionID;
      SnippetID.CollectionID := CollectionID;
      Depends[Idx] := SnippetID;
    end;
  end;

var
  Editor: IDatabaseEdit;      // object used to update user database
  Snippet: TSnippet;          // reference any existing snippet to overwrite
  SnippetInfo: TSnippetInfo;  // info about each snippet from import file
  ImportInfo: TImportInfo;    // info about how / whether to import a snippet
resourcestring
  // Error message
  sBadNameError = 'Can''t find snippet with key "%s" in import data';
begin
  Editor := Database as IDatabaseEdit;
  for SnippetInfo in fSnippetInfoList do
  begin
    if not fImportInfoList.FindByKey(SnippetInfo.Key, ImportInfo) then
      raise EBug.CreateFmt(sBadNameError, [SnippetInfo.Key]);

    if ImportInfo.Skip then
      Continue;

    AdjustDependsList(SnippetInfo.Data.Refs.Depends);

    Snippet := Database.Snippets.Find(ImportInfo.ImportAsKey, TCollectionID.__TMP__UserDBCollectionID);
    if Assigned(Snippet) then
      // snippet already exists: overwrite it
      Editor.UpdateSnippet(Snippet, SnippetInfo.Data)
    else
      // snippet is new: add to database
      Editor.AddSnippet(ImportInfo.ImportAsKey, SnippetInfo.Data);
  end;
end;

{ TImportInfo }

constructor TImportInfo.Create(const AOrigKey, AImportAsKey: string;
  const ASkip: Boolean);
begin
  fOrigKey := AOrigKey;
  fImportAsKey := AImportAsKey;
  fSkip := ASkip;
end;

{ TImportInfoComparer }

function TImportInfoComparer.Compare(const Left, Right: TImportInfo): Integer;
begin
  Result := TSnippetID.CompareKeys(Left.OrigKey, Right.OrigKey);
end;

{ TImportInfoList }

constructor TImportInfoList.Create;
begin
  inherited Create(TImportInfoComparer.Create);
end;

function TImportInfoList.FindByKey(const Key: string;
  out ImportInfo: TImportInfo): Boolean;
var
  Idx: Integer;   // index of named snippet in list
begin
  Idx := IndexOf(TImportInfo.Create(Key, ''));
  if Idx = -1 then
    Exit(False);
  ImportInfo := Items[Idx];
  Result := True;
end;

function TImportInfoList.IndexOfKey(const Key: string): Integer;
begin
  Result := IndexOf(TImportInfo.Create(Key, ''));
end;

end.

