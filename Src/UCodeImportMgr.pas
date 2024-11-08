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
  SysUtils,
  Generics.Collections,
  Generics.Defaults,
  // Project
  DB.UCollections,
  UCodeImportExport,
  UExceptions,
  UIStringList;


type
  ///  <summary>
  ///  Stores information about if and how a snippet is to be imported.
  ///  </summary>
  TImportInfo = record
  strict private
    // Property values
    fOrigKey: string;
    fDisplayName: string;
    fSkip: Boolean;
  public
    ///  <summary>Initialises properties to given values.</summary>
    constructor Create(const AOrigKey, ADisplayName: string;
      const ASkip: Boolean = False);
    ///  <summary>Snippet key per import file.</summary>
    property OrigKey: string read fOrigKey;
    ///  <summary>Snippet's display name.</summary>
    property DisplayName: string read fDisplayName;
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
    ///  <summary>Updates <c>Skip</c> property of a given list item.</summary>
    ///  <param name="AKey"><c>string</c> [in] Key that identifies list item to
    ///  be updated.</param>
    ///  <param name="AFlag"><c>Boolean</c> [in] Value to be stored in the given
    ///  list item's <c>Skip</c> property.</param>
    procedure SetSkip(const AKey: string; const AFlag: Boolean);
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
      ///  <summary>Value of RequestCollectionCallback property.</summary>
      fRequestCollectionCallback: TFunc<TCollectionID>;
    ///  <summary>Initialises import information list with details of snippets
    ///  read from import file.</summary>
    procedure InitImportInfoList;
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
    ///  <summary>Callback that gets the ID of the collection that will receive
    ///  the imported snippets.</summary>
    ///  <remarks>Defaults to the "user" collection ID if not assigned.
    ///  </remarks>
    property RequestCollectionCallback: TFunc<TCollectionID>
      read fRequestCollectionCallback write fRequestCollectionCallback;
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
  Classes,
  // Project
  ActiveText.UMain,
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
  // set default event handler
  fRequestCollectionCallback := function: TCollectionID
    begin
      {TODO -cCollections: Require a TCollections.DefaultCollection method or
              similar to replace the following __TMP__ method call.}
      Result := TCollectionID.__TMP__UserDBCollectionID;
    end;
end;

destructor TCodeImportMgr.Destroy;
begin
  fImportInfoList.Free;
  SetLength(fSnippetInfoList, 0);
  inherited;
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
        SnippetInfo.Key,
        StrIf(
          SnippetInfo.Data.Props.DisplayName = '',
          SnippetInfo.Key,
          SnippetInfo.Data.Props.DisplayName
        )
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
  CollectionID: TCollectionID;
  SnippetKey: string;
resourcestring
  // Error message
  sBadNameError = 'Can''t find snippet with key "%s" in import data';
begin
  Editor := Database as IDatabaseEdit;
  CollectionID := RequestCollectionCallback();
  for SnippetInfo in fSnippetInfoList do
  begin
    if not fImportInfoList.FindByKey(SnippetInfo.Key, ImportInfo) then
      raise EBug.CreateFmt(sBadNameError, [SnippetInfo.Key]);

    if ImportInfo.Skip then
      Continue;

    AdjustDependsList(SnippetInfo.Data.Refs.Depends);

    Snippet := Database.Snippets.Find(ImportInfo.OrigKey, CollectionID);
    if Assigned(Snippet) then
      SnippetKey := (Database as IDatabaseEdit).GetUniqueSnippetKey(
        CollectionID
      )
    else
      SnippetKey := ImportInfo.OrigKey;
    Editor.AddSnippet(SnippetKey, SnippetInfo.Data);
    {TODO -cVault: Reintroduce the option to overwrite a snippet with matching
            ID, but allow user to select whether this can happen.}
  end;
end;

{ TImportInfo }

constructor TImportInfo.Create(const AOrigKey, ADisplayName: string;
  const ASkip: Boolean = False);
begin
  fOrigKey := AOrigKey;
  fSkip := ASkip;
  fDisplayName := ADisplayName;
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
  Idx := IndexOfKey(Key);
  if Idx = -1 then
    Exit(False);
  ImportInfo := Items[Idx];
  Result := True;
end;

function TImportInfoList.IndexOfKey(const Key: string): Integer;
begin
  Result := IndexOf(TImportInfo.Create(Key, ''));
end;

procedure TImportInfoList.SetSkip(const AKey: string; const AFlag: Boolean);
const
  // Do not localise
  sKeyNotFound = 'Snippet key "%s" not found while setting import skip flag';
var
  ImportInfo: TImportInfo;
  Idx: Integer;
begin
  Idx := IndexOfKey(AKey);
  if Idx < 0 then
    raise EBug.CreateFmt(sKeyNotFound, [AKey]);
  ImportInfo := Items[Idx];
  ImportInfo.Skip := AFlag;
  Items[Idx] := ImportInfo;
end;

end.

