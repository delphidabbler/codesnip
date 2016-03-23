{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2016, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides classes that encapsulate snippets and associated lists.
}


unit CS.Database.Snippets;

interface

uses
  // Library
  Collections.Base,
  Collections.Sets,
  // Project
  CS.ActiveText,
  CS.Database.Types,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  Compilers.UGlobals,
  IntfCommon,
  UComparers,
  UExceptions,
  UIStringList;

type
  ///  <summary>Comparer for snippets by title.</summary>
  TSnippetTitleComparator = class(TComparator<ISnippet>)
  public
    ///  <summary>Compares snippets Left and Right. Returns -ve if Left's
    ///  title sorts before Right's, 0 if the same or +ve if Left's title is
    ///  greater than Right's.</summary>
    function Compare(const Left, Right: ISnippet): Integer; override;
    function Equals(const Left, Right: ISnippet): Boolean; override;
    function GetHashCode(const Snippet: ISnippet): Integer; override;
  end;

type
  TSnippetBase = class(TInterfacedObject)
  public
    const
      MaxTitleLength = 64;
  strict private
    var
      fID: TSnippetID;
      fCreated: TUTCDateTime;
      fModified: TUTCDateTime;
      fTitle: string;
      fDescription: IActiveText;
      fSourceCode: string;
      fLanguageID: TSourceCodeLanguageID;
      fRequiredModules: IStringList;
      fRequiredSnippets: ISnippetIDList;
      fXRefs: ISnippetIDList;
      fNotes: IActiveText;
      fKindID: TSnippetKindID;
      fCompileResults: TCompileResults;
      fTags: ITagSet;
      fOrigin: ISnippetOrigin;
      fTestInfo: TSnippetTestInfo;
      fStarred: Boolean;
  public
    constructor Create; overload;
    constructor Create(const ASnippetID: TSnippetID); overload;
    constructor Create(const ASourceSnippet: TSnippetBase); overload;
    destructor Destroy; override;
    procedure UpdateFrom(const ASourceSnippet: TSnippetBase);
    function CanCompile: Boolean;
    function GetID: TSnippetID; virtual;
    function GetCreated: TUTCDateTime;
    procedure SetCreated(const Value: TUTCDateTime);
    function GetModified: TUTCDateTime; virtual;
    procedure SetModified(const Value: TUTCDateTime);
    function GetTitle: string; virtual;
    procedure SetTitle(const Value: string);
    function GetDescription: IActiveText; virtual;
    procedure SetDescription(Value: IActiveText);
    function GetSourceCode: string; virtual;
    procedure SetSourceCode(const Value: string);
    function GetLanguageID: TSourceCodeLanguageID; virtual;
    procedure SetLanguageID(const Value: TSourceCodeLanguageID);
    function GetRequiredModules: IStringList; virtual;
    procedure SetRequiredModules(Value: IStringList);
    function GetRequiredSnippets: ISnippetIDList; virtual;
    procedure SetRequiredSnippets(Value: ISnippetIDList);
    function GetXRefs: ISnippetIDList; virtual;
    procedure SetXRefs(Value: ISnippetIDList);
    function GetNotes: IActiveText; virtual;
    procedure SetNotes(Value: IActiveText);
    function GetKindID: TSnippetKindID; virtual;
    procedure SetKindID(const Value: TSnippetKindID);
    function GetCompileResults: TCompileResults; virtual;
    procedure SetCompileResults(const Value: TCompileResults);
    function GetTags: ITagSet; virtual;
    procedure SetTags(Value: ITagSet);
    function GetOrigin: ISnippetOrigin; virtual;
    procedure SetOrigin(AOrigin: ISnippetOrigin);
    function GetTestInfo: TSnippetTestInfo; virtual;
    procedure SetTestInfo(ATestInfo: TSnippetTestInfo);
    function GetStarred: Boolean; virtual;
    procedure SetStarred(AStarred: Boolean);
  end;

  TEditableSnippet = class(TSnippetBase, IEditableSnippet)
  public
    class function CreateNew: TEditableSnippet;
    destructor Destroy; override;
    procedure UpdateFrom(AOther: IEditableSnippet); overload;
    procedure UpdateFrom(AOther: ISnippet); overload;
    function CloneAsReadOnly: ISnippet;
  end;

  TReadOnlySnippet = class(TSnippetBase, ISnippet);

  EDBSnippet = class(EBug);

  // TODO: Add new ToList<TSnippetID> method
  ///  <summary>Implements a list of snippet identification records.</summary>
  TSnippetIDList = class(
    TInterfacedObject, ISnippetIDList, IAssignable, IClonable
  )
  strict private
    var
      ///  <summary>Internal list if snippet ID records.</summary>
      fList: TLinkedSet<TSnippetID>;
  public
    ///  <summary>Constructs empty snippet ID list object.</summary>
    ///  <param name="ACapacity"><c>Integer</c> [in] Initial capacity of list.
    ///  Omitting this parameter, or passing a value of zero or lower causes the
    ///  default capacity to be used.</param>
    constructor Create(const ACapacity: Integer = 0);

    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;

    ///  <summary>Gets new list enumerator.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function GetEnumerator: IEnumerator<TSnippetID>;

    ///  <summary>Clears the list.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    procedure Clear;

    ///  <summary>Adds given snippet ID to list and returns its index in list.
    ///  </summary>
    ///  <remarks>
    ///  <para>If SnippetID is already in list then nothing happens.</para>
    ///  <para>Method of ISnippetIDList.</para>
    ///  </remarks>
    procedure Add(const SnippetID: TSnippetID);

    ///  <summary>Removed the given snippet ID from the list.</summary>
    ///  <remarks>
    ///  <para>Does nothing if SnippetID is not in the list.</para>
    ///  <para>Method of ISnippetIDList.</para>
    ///  </remarks>
    procedure Remove(const SnippetID: TSnippetID);

    ///  <summary>Checks if list contains given snippet ID.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Contains(const SnippetID: TSnippetID): Boolean;

    ///  <summary>Checks if this list has the same content as another list.
    ///  </summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function IsEqual(Other: ISnippetIDList): Boolean;

    ///  <summary>Checks if list is empty.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function IsEmpty: Boolean;

    ///  <summary>Returns number of snippet ID records in list.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Count: Integer;

    ///  <summary>Copies properties of given list to this one.</summary>
    ///  <param name="Src">IInterface [in] List whose properties are to be
    ///  copied. Src must support ISnippetIDList.</param>
    ///  <remarks>Method of IAssignable.</remarks>
    procedure Assign(const Src: IInterface);

    ///  <summary>Creates and returns a new list that is an exact copy of the
    ///  current one.</summary>
    ///  <returns>IInterface. Reference to cloned object.</returns>
    ///  <remarks>Method of IClonable</remarks>
    function Clone: IInterface;
  end;

implementation

uses
  // Delphi
  SysUtils,
  Generics.Defaults,
  Classes,
  RTLConsts,
  // Project
  CS.Database.SnippetOrigins,
  CS.Database.Tags,
  CS.Utils.Hashes,
  UStrUtils;

{ TSnippetIDList }

procedure TSnippetIDList.Add(const SnippetID: TSnippetID);
begin
  if not fList.Contains(SnippetID) then
    fList.Add(SnippetID);
end;

procedure TSnippetIDList.Assign(const Src: IInterface);
var
  SrcID: TSnippetID;  // references each ID in source
  SrcList: ISnippetIDList;
begin
  if not Supports(Src, ISnippetIDList, SrcList) then
    raise EBug.Create(ClassName + '.Assign: Src must support ISnippetIDList');
  Clear;
  for SrcID in SrcList do
    Add(SrcID);
end;

procedure TSnippetIDList.Clear;
begin
  fList.Clear;
end;

function TSnippetIDList.Clone: IInterface;
begin
  Result := TSnippetIDList.Create;
  (Result as IAssignable).Assign(Self);
end;

function TSnippetIDList.Contains(const SnippetID: TSnippetID): Boolean;
begin
  Result := fList.Contains(SnippetID);
end;

function TSnippetIDList.Count: Integer;
begin
  Result := fList.Count;
end;

constructor TSnippetIDList.Create(const ACapacity: Integer);
begin
  fList := TLinkedSet<TSnippetID>.Create(
    TRulesFactory<TSnippetID>.CreateFromComparator(
      TSnippetID.TComparator.Create
    ),
    ACapacity
  );
end;

destructor TSnippetIDList.Destroy;
begin
  fList.Free;
  inherited;
end;

function TSnippetIDList.GetEnumerator: IEnumerator<TSnippetID>;
begin
  Result := fList.GetEnumerator;
end;

function TSnippetIDList.IsEmpty: Boolean;
begin
  Result := fList.Count = 0;
end;

function TSnippetIDList.IsEqual(Other: ISnippetIDList): Boolean;
var
  OtherID: TSnippetID;
begin
  Assert(Assigned(Other), ClassName + '.IsEqual: Other is nil');
  // Lists are equal if they are the same size and every member of Other is in
  // this list.
  if fList.Count <> Other.Count then
    Exit(False);
  for OtherID in Other do
    if not fList.Contains(OtherID) then
      Exit(False);
  Result := True;
end;

procedure TSnippetIDList.Remove(const SnippetID: TSnippetID);
begin
  fList.Remove(SnippetID);
end;

{ TSnippetBase }

function TSnippetBase.CanCompile: Boolean;
begin
  Result := (fKindID <> skFreeform)
    and TSourceCodeLanguage.HasExtendedSupport(fLanguageID);
end;

constructor TSnippetBase.Create(const ASourceSnippet: TSnippetBase);
begin
  Create(ASourceSnippet.fID);
  UpdateFrom(ASourceSnippet);
end;

constructor TSnippetBase.Create(const ASnippetID: TSnippetID);
var
  CompID: TCompilerID;
begin
  inherited Create;
  fID := ASnippetID;
  fCreated := TUTCDateTime.Now;
  fModified := fCreated;
  fTitle := '';
  fDescription := nil;
  fSourceCode := '';
  fLanguageID := TSourceCodeLanguageID.CreateDefault;
  fRequiredModules := nil;
  fRequiredSnippets := nil;
  fXRefs := nil;
  fNotes := nil;
  fKindID := skFreeForm;
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    fCompileResults[CompID] := crQuery;
  fTags := nil;
  fOrigin := nil;
  fTestInfo := stiNone;
  fStarred := False;
end;

constructor TSnippetBase.Create;
begin
  raise ENoConstructException.CreateFmt(
    'Parameterless constructor not permitted for %s', [ClassName]
  );
end;

destructor TSnippetBase.Destroy;
begin
  inherited;
end;

function TSnippetBase.GetCompileResults: TCompileResults;
begin
  Result := fCompileResults;
end;

function TSnippetBase.GetCreated: TUTCDateTime;
begin
  Result := fCreated;
end;

function TSnippetBase.GetDescription: IActiveText;
begin
  if not Assigned(fDescription) then
    Exit(TActiveTextFactory.CreateActiveText);
  Result := TActiveTextFactory.CloneActiveText(fDescription);
end;

function TSnippetBase.GetID: TSnippetID;
begin
  Result := fID;
end;

function TSnippetBase.GetKindID: TSnippetKindID;
begin
  Result := fKindID;
end;

function TSnippetBase.GetLanguageID: TSourceCodeLanguageID;
begin
  Result := fLanguageID;
end;

function TSnippetBase.GetModified: TUTCDateTime;
begin
  Result := fModified;
end;

function TSnippetBase.GetNotes: IActiveText;
begin
  if not Assigned(fNotes) then
    Exit(TActiveTextFactory.CreateActiveText);
  Result := TActiveTextFactory.CloneActiveText(fNotes);
end;

function TSnippetBase.GetOrigin: ISnippetOrigin;
begin
  if not Assigned(fOrigin) then
    Exit(TLocalSnippetOrigin.Create);
  Result := TRemoteSnippetOrigin.Create(fOrigin);
end;

function TSnippetBase.GetRequiredModules: IStringList;
begin
  if not Assigned(fRequiredModules) then
    Exit(TIStringList.Create);
  Result := TIStringList.Create(fRequiredModules);
end;

function TSnippetBase.GetRequiredSnippets: ISnippetIDList;
begin
  if not Assigned(fRequiredSnippets) then
    Exit(TSnippetIDList.Create);
  Result := (fRequiredSnippets as IClonable).Clone as ISnippetIDList;
end;

function TSnippetBase.GetSourceCode: string;
begin
  Result := fSourceCode;
end;

function TSnippetBase.GetStarred: Boolean;
begin
  Result := fStarred;
end;

function TSnippetBase.GetTags: ITagSet;
begin
  if not Assigned(fTags) then
    Exit(TTagSet.Create);
  Result := TTagSet.Create(fTags);
end;

function TSnippetBase.GetTestInfo: TSnippetTestInfo;
begin
  Result := fTestInfo;
end;

function TSnippetBase.GetTitle: string;
begin
  Result := fTitle;
end;

function TSnippetBase.GetXRefs: ISnippetIDList;
begin
  if not Assigned(fXRefs) then
    Exit(TSnippetIDList.Create);
  Result := (fXRefs as IClonable).Clone as ISnippetIDList;
end;

procedure TSnippetBase.SetCompileResults(const Value: TCompileResults);
begin
  fCompileResults := Value;
end;

procedure TSnippetBase.SetCreated(const Value: TUTCDateTime);
begin
  fCreated := Value;
end;

procedure TSnippetBase.SetDescription(Value: IActiveText);
begin
  if not Assigned(Value) or Value.IsEmpty then
    fDescription := nil
  else
    fDescription := TActiveTextFactory.CloneActiveText(Value);
end;

procedure TSnippetBase.SetKindID(const Value: TSnippetKindID);
begin
  fKindID := Value;
end;

procedure TSnippetBase.SetLanguageID(const Value: TSourceCodeLanguageID);
begin
  fLanguageID := Value;
end;

procedure TSnippetBase.SetModified(const Value: TUTCDateTime);
begin
  fModified := Value;
end;

procedure TSnippetBase.SetNotes(Value: IActiveText);
begin
  if not Assigned(Value) or Value.IsEmpty then
    fNotes := nil
  else
    fNotes := TActiveTextFactory.CloneActiveText(Value);
end;

procedure TSnippetBase.SetOrigin(AOrigin: ISnippetOrigin);
begin
  if not Assigned(AOrigin) then
    fOrigin := nil
  else
    fOrigin := TRemoteSnippetOrigin.Create(AOrigin);
end;

procedure TSnippetBase.SetRequiredModules(Value: IStringList);
begin
  if not Assigned(Value) or Value.IsEmpty then
    fRequiredModules := nil
  else
    fRequiredModules := TIStringList.Create(Value);
end;

procedure TSnippetBase.SetRequiredSnippets(Value: ISnippetIDList);
begin
  if not Assigned(Value) or Value.IsEmpty then
    fRequiredSnippets := nil
  else
    fRequiredSnippets := (Value as IClonable).Clone as ISnippetIDList;
end;

procedure TSnippetBase.SetSourceCode(const Value: string);
begin
  fSourceCode := Value;
end;

procedure TSnippetBase.SetStarred(AStarred: Boolean);
begin
  fStarred := AStarred;
end;

procedure TSnippetBase.SetTags(Value: ITagSet);
begin
  if not Assigned(Value) or Value.IsEmpty then
    fTags := nil
  else
    fTags := TTagSet.Create(Value);
end;

procedure TSnippetBase.SetTestInfo(ATestInfo: TSnippetTestInfo);
begin
  fTestInfo := ATestInfo;
end;

procedure TSnippetBase.SetTitle(const Value: string);
begin
  if Length(Value) > MaxTitleLength then
    fTitle := StrSliceLeft(Value, MaxTitleLength)
  else
    fTitle := Value;
end;

procedure TSnippetBase.SetXRefs(Value: ISnippetIDList);
begin
  if not Assigned(Value) or Value.IsEmpty then
    fXRefs := nil
  else
    fXRefs := (Value as IClonable).Clone as ISnippetIDList;
end;

procedure TSnippetBase.UpdateFrom(const ASourceSnippet: TSnippetBase);
begin
  // Never change snippet ID
  SetCreated(ASourceSnippet.fCreated);
  SetModified(ASourceSnippet.fModified);
  SetTitle(ASourceSnippet.fTitle);
  SetDescription(ASourceSnippet.fDescription);
  SetSourceCode(ASourceSnippet.fSourceCode);
  SetLanguageID(ASourceSnippet.fLanguageID);
  SetRequiredModules(ASourceSnippet.fRequiredModules);
  SetRequiredSnippets(ASourceSnippet.fRequiredSnippets);
  SetXRefs(ASourceSnippet.fXRefs);
  SetNotes(ASourceSnippet.fNotes);
  SetKindID(ASourceSnippet.fKindID);
  SetCompileResults(ASourceSnippet.fCompileResults);
  SetTags(ASourceSnippet.fTags);
  SetOrigin(ASourceSnippet.fOrigin);
  SetTestInfo(ASourceSnippet.fTestInfo);
  SetStarred(ASourceSnippet.fStarred);
end;

{ TEditableSnippet }

function TEditableSnippet.CloneAsReadOnly: ISnippet;
begin
  Result := TReadOnlySnippet.Create(Self);
end;

class function TEditableSnippet.CreateNew: TEditableSnippet;
begin
  Result := TEditableSnippet.Create(TSnippetID.CreateNew);
end;

destructor TEditableSnippet.Destroy;
begin
  inherited;
end;

procedure TEditableSnippet.UpdateFrom(AOther: IEditableSnippet);
begin
  UpdateFrom(AOther as TEditableSnippet);
end;

procedure TEditableSnippet.UpdateFrom(AOther: ISnippet);
begin
  UpdateFrom(AOther as TReadOnlySnippet);
end;

{ TSnippetTitleComparator }

function TSnippetTitleComparator.Compare(const Left, Right: ISnippet): Integer;
begin
  Result := StrCompareText(Left.Title, Right.Title);
end;

function TSnippetTitleComparator.Equals(const Left, Right: ISnippet): Boolean;
begin
  Result := StrSameText(Left.Title, Right.Title);
end;

function TSnippetTitleComparator.GetHashCode(const Snippet: ISnippet): Integer;
begin
  Result := TextHash(Snippet.Title);
end;

end.

