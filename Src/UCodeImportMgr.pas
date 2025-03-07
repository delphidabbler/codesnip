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
  DB.Vaults,
  DB.IO.ImportExport.CS4,
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
    fNewKey: string;
    fDisplayName: string;
    fSkip: Boolean;
  public
    ///  <summary>Initialises properties to given values.</summary>
    constructor Create(const AOrigKey, ANewKey, ADisplayName: string;
      const ASkip: Boolean = False);
    ///  <summary>Snippet key per import file.</summary>
    property OrigKey: string read fOrigKey;
    ///  <sumamry>New, unique snippet key under which the snippet will be
    ///  imported.</summary>
    property NewKey: string read fNewKey;
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
      ///  <summary>Value of <c>RequestVaultCallback</c> property.</summary>
      fRequestVaultCallback: TFunc<TVaultID>;
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
    ///  described by ImportInfo property, using the vault specified in
    ///  <c>RequestVaultCallback</c>.</summary>
    ///  <remarks>Any snippets referenced in the an imported snippet's
    ///  <c>Depends</c> or <c>XRefs</c> property must also be included in the
    ///  import otherwise the snippet is stripped from the dependency list.
    ///  </remarks>
    procedure UpdateDatabase;
    ///  <summary>List of information describing if and how to import snippets
    ///  in import file. Permits customisation of import.</summary>
    property ImportInfo: TImportInfoList read fImportInfoList;
    ///  <summary>Callback that gets the ID of the vault that will receive the
    ///  imported snippets.</summary>
    ///  <remarks>Defaults to the default vault ID if not assigned.</remarks>
    property RequestVaultCallback: TFunc<TVaultID>
      read fRequestVaultCallback write fRequestVaultCallback;
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
  DB.Main,
  DB.SnippetIDs,
  DB.Snippets,
  IntfCommon,
  UIOUtils,
  UStrUtils;


{ TCodeImportMgr }

constructor TCodeImportMgr.Create;
begin
  inherited Create;
  SetLength(fSnippetInfoList, 0);
  fImportInfoList := TImportInfoList.Create;
  // set default event handler
  fRequestVaultCallback := function: TVaultID
    begin
      Result := TVaultID.Default;
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
    TCS4SnippetImporter.ImportData(fSnippetInfoList, Data);
  except
    on E: EStreamError do
      raise ECodeImportMgr.Create(E);
    on E: ECS4SnippetImporter do
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
        (Database as IDatabaseEdit).GetUniqueSnippetKey(RequestVaultCallback),
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

  // Adjusts a snippet's references list to exclude snippets not included in the
  // import.
  function AdjustRefsList(const ARefs: ISnippetIDList): ISnippetIDList;
  var
    SnippetID: TSnippetID;  // each snippet ID in dependency list
    Info: TImportInfo;
  begin
    // We only include snippets in depends list if it is included in the import
    Result := TSnippetIDList.Create;
    for SnippetID in ARefs do
    begin
      if fImportInfoList.FindByKey(SnippetID.Key, Info) and not Info.Skip then
        Result.Add(TSnippetID.Create(Info.NewKey, SnippetID.VaultID));
    end;
  end;

type
  // Record used to save a snippet's references
  TSavedReferences = record
    Snippet: TSnippet;
    Data: TSnippetEditData;
  end;

var
  Editor: IDatabaseEdit;                  // object used to update user database
  SnippetInfo: TSnippetInfo;         // info about each snippet from import file
  ImportInfo: TImportInfo;       // info about how / whether to import a snippet
  VaultID: TVaultID;                         // vault into which we're importing
  SavedRefs: TList<TSavedReferences>;   // preserved references for each snippet
  SavedRef: TSavedReferences;                        // each record in Refs list
  SnippetDataNoRefs: TSnippetEditData;   // snippet data with references cleared
resourcestring
  // Error message
  sBadNameError = 'Can''t find snippet with key "%s" in import data';
begin
  {TODO -cRefactor: Tidy up messy use of both fSnippetInfoList and
          fImportInfoList: include all required info in fImportInfoList?
  }

  Editor := Database as IDatabaseEdit;
  VaultID := RequestVaultCallback();

  SavedRefs := TList<TSavedReferences>.Create(
    TDelegatedComparer<TSavedReferences>.Create(
      function (const Left, Right: TSavedReferences): Integer
      begin
        Result := Left.Snippet.CompareTo(Right.Snippet);
      end
    )
  );
  try
    for SnippetInfo in fSnippetInfoList do
    begin
      if not fImportInfoList.FindByKey(SnippetInfo.Key, ImportInfo) then
        raise EBug.CreateFmt(sBadNameError, [SnippetInfo.Key]);

      if ImportInfo.Skip then
        Continue;

      // Exclude snippet from depends list if snippet not included in import.
      (SnippetInfo.Data.Refs.Depends as IAssignable).Assign(
        AdjustRefsList(SnippetInfo.Data.Refs.Depends)
      );

      // store snippet data with references
      SavedRef.Data.Assign(SnippetInfo.Data);

      // clear references before adding snippet: it will probably delete most
      // anyway if reference is to a snippet after this one in the import list
      SnippetDataNoRefs.Assign(SnippetInfo.Data);
      // .. XRef should be clear regardless, because XRefs not included in
      //    export files.
      SnippetDataNoRefs.Refs.XRef.Clear;
      SnippetDataNoRefs.Refs.Depends.Clear;

      // add snippet without any dependency
      SavedRef.Snippet := Editor.AddSnippet(
        ImportInfo.NewKey, VaultID, SnippetDataNoRefs
      );

      // save snippet with its dependencies
      SavedRefs.Add(SavedRef);
    end;

    // Add back the saved snippet references
    for SavedRef in SavedRefs do
      if SavedRef.Data.Refs.Depends.Count > 0 then
        Editor.UpdateSnippet(SavedRef.Snippet, SavedRef.Data);

  finally
    SavedRefs.Free;
  end;

end;

{ TImportInfo }

constructor TImportInfo.Create(const AOrigKey, ANewKey, ADisplayName: string;
  const ASkip: Boolean = False);
begin
  fOrigKey := AOrigKey;
  fNewKey := ANewKey;
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
  Result := IndexOf(TImportInfo.Create(Key, '', ''));
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

