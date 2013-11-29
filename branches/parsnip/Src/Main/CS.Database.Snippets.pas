{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides classes that encapsulate snippets and associated lists.
}


unit CS.Database.Snippets;

interface

uses
  Generics.Collections,
  CS.ActiveText,
  Compilers.UGlobals,
  CS.Database.Types,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  IntfCommon,
  UExceptions,
  UIStringList;

type
  TSnippetBase = class(TInterfacedObject)
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
      fKind: TSnippetKind;
      fCompileResults: TCompileResults;
      fTags: ITagSet;
      fLinkInfo: ISnippetLinkInfo;
      fTestInfo: TSnippetTestInfo;
      fStarred: Boolean;
  strict protected
    function SupportsProperty(const APropID: TDBSnippetProp): Boolean; virtual;
  public
    constructor Create; overload;
    constructor Create(const ASnippetID: TSnippetID); overload;
    constructor Create(const ASourceSnippet: TSnippetBase); overload;
    destructor Destroy; override;
    procedure UpdateFrom(const ASourceSnippet: TSnippetBase);
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
    function GetKind: TSnippetKind; virtual;
    procedure SetKind(const Value: TSnippetKind);
    function GetCompileResults: TCompileResults; virtual;
    procedure SetCompileResults(const Value: TCompileResults);
    function GetTags: ITagSet; virtual;
    procedure SetTags(Value: ITagSet);
    function GetLinkInfo: ISnippetLinkInfo; virtual;
    procedure SetLinkInfo(ALinkInfo: ISnippetLinkInfo);
    function GetTestInfo: TSnippetTestInfo; virtual;
    procedure SetTestInfo(ATestInfo: TSnippetTestInfo);
    function GetStarred: Boolean; virtual;
    procedure SetStarred(AStarred: Boolean);
  end;

  // TODO: rename back to TSnippet once ambiguity with legacy TSnippet resolved.
  TNewSnippet = class(TSnippetBase, ISnippet)
  public
    class function CreateNew: TNewSnippet;
    destructor Destroy; override;
  end;

  TPartialSnippet = class(TSnippetBase, IReadOnlySnippet)
  strict private
    var
      fValidProperties: TDBSnippetProps;
    procedure CheckValidProp(const AProp: TDBSnippetProp);
  public
    constructor Create(const ASnippetID: TSnippetID); overload;
    constructor Create(const ASourceSnippet: TSnippetBase;
      const ValidProps: TDBSnippetProps = []); overload;
    destructor Destroy; override;
    function GetModified: TUTCDateTime; override;
    function GetTitle: string; override;
    function GetDescription: IActiveText; override;
    function GetSourceCode: string; override;
    function GetLanguageID: TSourceCodeLanguageID; override;
    function GetRequiredModules: IStringList; override;
    function GetRequiredSnippets: ISnippetIDList; override;
    function GetXRefs: ISnippetIDList; override;
    function GetNotes: IActiveText; override;
    function GetKind: TSnippetKind; override;
    function GetCompileResults: TCompileResults; override;
    function GetTags: ITagSet; override;
    function GetLinkInfo: ISnippetLinkInfo; override;
    function GetTestInfo: TSnippetTestInfo; override;
    function GetStarred: Boolean; override;

    function GetValidProperties: TDBSnippetProps;
    function SupportsProperty(const AProp: TDBSnippetProp): Boolean; override;
  end;

  EDBSnippet = class(EBug);

  // TODO: Add new ToList<TSnippetID> method
  ///  <summary>Implements a list of snippet identification records.</summary>
  TSnippetIDList = class(
    TInterfacedObject, ISnippetIDList, IAssignable, IClonable
  )
  strict private
    var
      // TODO: Change implementation to use DelphiColl - use TLinkedSet
      ///  <summary>Internal list if snippet ID records.</summary>
      fList: TList<TSnippetID>;
  public
    ///  <summary>Constructs empty list object.</summary>
    constructor Create; overload;

    ///  <summary>Constructs empty list object with the given capacity.
    ///  </summary>
    constructor Create(const ACapacity: Integer); overload;

    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;

    ///  <summary>Gets new list enumerator.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function GetEnumerator: TEnumerator<TSnippetID>;

    ///  <summary>Clears the list.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    procedure Clear;

    ///  <summary>Adds given snippet ID to list and returns its index in list.
    ///  </summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Add(const SnippetID: TSnippetID): Integer;

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
  SysUtils,
  Generics.Defaults,
  Classes,
  RTLConsts,
  CS.Database.SnippetLinks,
  CS.Database.Tags,
  USnippetIDs;

{ TSnippetIDList }

function TSnippetIDList.Add(const SnippetID: TSnippetID): Integer;
begin
  Result := fList.Add(SnippetID);
end;

procedure TSnippetIDList.Assign(const Src: IInterface);
var
  SrcID: TSnippetID;  // references each ID in source
  SrcList: ISnippetIDList;
begin
  if not Supports(Src, ISnippetIDList, SrcList) then
    raise EBug.Create(ClassName + '.Assign: Src must support ISnippetIDList');
  Clear;
  fList.Capacity := SrcList.Count;
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
  Create;
  fList.Capacity := ACapacity;
end;

constructor TSnippetIDList.Create;
begin
  inherited;
  fList := TList<TSnippetID>.Create(
    TDelegatedComparer<TSnippetID>.Create(
      function(const Left, Right: TSnippetID): Integer
      begin
        Result := TSnippetID.Compare(Left, Right);
      end
    )
  );
end;

destructor TSnippetIDList.Destroy;
begin
  fList.Free;
  inherited;
end;

function TSnippetIDList.GetEnumerator: TEnumerator<TSnippetID>;
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
  fKind := skFreeForm;
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    fCompileResults[CompID] := crQuery;
  fTags := nil;
  fLinkInfo := nil;
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

function TSnippetBase.GetKind: TSnippetKind;
begin
  Result := fKind;
end;

function TSnippetBase.GetLanguageID: TSourceCodeLanguageID;
begin
  Result := fLanguageID;
end;

function TSnippetBase.GetLinkInfo: ISnippetLinkInfo;
begin
  if not Assigned(fLinkInfo) then
    Exit(TNullSnippetLinkInfo.Create);
  Result := TSnippetLinkInfo.Create(fLinkInfo);
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

procedure TSnippetBase.SetKind(const Value: TSnippetKind);
begin
  fKind := Value;
end;

procedure TSnippetBase.SetLanguageID(const Value: TSourceCodeLanguageID);
begin
  fLanguageID := Value;
end;

procedure TSnippetBase.SetLinkInfo(ALinkInfo: ISnippetLinkInfo);
begin
  if not Assigned(ALinkInfo) then
    fLinkInfo := nil
  else
    fLinkInfo := TSnippetLinkInfo.Create(ALinkInfo);
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

procedure TSnippetBase.SetRequiredModules(Value: IStringList);
begin
  if not Assigned(Value) or (Value.Count = 0) then
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
  fTitle := Value;
end;

procedure TSnippetBase.SetXRefs(Value: ISnippetIDList);
begin
  if not Assigned(Value) or Value.IsEmpty then
    fXRefs := nil
  else
    fXRefs := (Value as IClonable).Clone as ISnippetIDList;
end;

function TSnippetBase.SupportsProperty(const APropID: TDBSnippetProp): Boolean;
begin
  Result := True;
end;

procedure TSnippetBase.UpdateFrom(const ASourceSnippet: TSnippetBase);
begin
  Assert(fID = ASourceSnippet.fID,
    ClassName + '.UpdateFrom: source snippet must have same ID');
  if SupportsProperty(spCreated) then
    SetCreated(ASourceSnippet.fCreated);
  if SupportsProperty(spModified) then
    SetModified(ASourceSnippet.fModified);
  if SupportsProperty(spTitle) then
    SetTitle(ASourceSnippet.fTitle);
  if SupportsProperty(spDescription) then
    SetDescription(ASourceSnippet.fDescription);
  if SupportsProperty(spSourceCode) then
    SetSourceCode(ASourceSnippet.fSourceCode);
  if SupportsProperty(spLanguageID) then
    SetLanguageID(ASourceSnippet.fLanguageID);
  if SupportsProperty(spRequiredModules) then
    SetRequiredModules(ASourceSnippet.fRequiredModules);
  if SupportsProperty(spRequiredSnippets) then
    SetRequiredSnippets(ASourceSnippet.fRequiredSnippets);
  if SupportsProperty(spXRefs) then
    SetXRefs(ASourceSnippet.fXRefs);
  if SupportsProperty(spNotes) then
    SetNotes(ASourceSnippet.fNotes);
  if SupportsProperty(spKind) then
    SetKind(ASourceSnippet.fKind);
  if SupportsProperty(spCompileResults) then
    SetCompileResults(ASourceSnippet.fCompileResults);
  if SupportsProperty(spTags) then
    SetTags(ASourceSnippet.fTags);
  if SupportsProperty(spLinkInfo) then
    SetLinkInfo(ASourceSnippet.fLinkInfo);
  if SupportsProperty(spTestInfo) then
    SetTestInfo(ASourceSnippet.fTestInfo);
  if SupportsProperty(spStarred) then
    SetStarred(ASourceSnippet.fStarred);
end;

{ TNewSnippet }

class function TNewSnippet.CreateNew: TNewSnippet;
begin
  Result := TNewSnippet.Create(TSnippetID.CreateNew);
end;

destructor TNewSnippet.Destroy;
begin
  inherited;
end;

{ TPartialSnippet }

procedure TPartialSnippet.CheckValidProp(const AProp: TDBSnippetProp);
begin
  if not SupportsProperty(AProp) then
    raise EDBSnippet.Create('Property access not permitted');
end;

constructor TPartialSnippet.Create(const ASnippetID: TSnippetID);
begin
  raise ENoConstructException.CreateFmt(
    'This form of constructor not permitted for %s', [ClassName]
  );
end;

constructor TPartialSnippet.Create(const ASourceSnippet: TSnippetBase;
  const ValidProps: TDBSnippetProps);
begin
  fValidProperties := ValidProps;
  inherited Create(ASourceSnippet);
end;

destructor TPartialSnippet.Destroy;
begin
  inherited;
end;

function TPartialSnippet.GetCompileResults: TCompileResults;
begin
  CheckValidProp(spCompileResults);
  Result := inherited GetCompileResults;
end;

function TPartialSnippet.GetDescription: IActiveText;
begin
  CheckValidProp(spDescription);
  Result := inherited GetDescription;
end;

function TPartialSnippet.GetKind: TSnippetKind;
begin
  CheckValidProp(spKind);
  Result := inherited GetKind;
end;

function TPartialSnippet.GetLanguageID: TSourceCodeLanguageID;
begin
  CheckValidProp(spLanguageID);
  Result := inherited GetLanguageID;
end;

function TPartialSnippet.GetLinkInfo: ISnippetLinkInfo;
begin
  CheckValidProp(spLinkInfo);
  Result := inherited GetLinkInfo;
end;

function TPartialSnippet.GetModified: TUTCDateTime;
begin
  CheckValidProp(spModified);
  Result := inherited GetModified;
end;

function TPartialSnippet.GetNotes: IActiveText;
begin
  CheckValidProp(spNotes);
  Result := inherited GetNotes;
end;

function TPartialSnippet.GetRequiredModules: IStringList;
begin
  CheckValidProp(spRequiredModules);
  Result := inherited GetRequiredModules;
end;

function TPartialSnippet.GetRequiredSnippets: ISnippetIDList;
begin
  CheckValidProp(spRequiredSnippets);
  Result := inherited GetRequiredSnippets;
end;

function TPartialSnippet.GetSourceCode: string;
begin
  CheckValidProp(spSourceCode);
  Result := inherited GetSourceCode;
end;

function TPartialSnippet.GetStarred: Boolean;
begin
  CheckValidProp(spStarred);
  Result := inherited GetStarred;
end;

function TPartialSnippet.GetTags: ITagSet;
begin
  CheckValidProp(spTags);
  Result := inherited GetTags;
end;

function TPartialSnippet.GetTestInfo: TSnippetTestInfo;
begin
  CheckValidProp(spTestInfo);
  Result := inherited GetTestInfo;
end;

function TPartialSnippet.GetTitle: string;
begin
  CheckValidProp(spTitle);
  Result := inherited GetTitle;
end;

function TPartialSnippet.GetValidProperties: TDBSnippetProps;
begin
  Result := fValidProperties;
end;

function TPartialSnippet.GetXRefs: ISnippetIDList;
begin
  CheckValidProp(spXRefs);
  Result := inherited GetXRefs;
end;

function TPartialSnippet.SupportsProperty(const AProp: TDBSnippetProp): Boolean;
begin
  Result := (AProp in fValidProperties) or (fValidProperties = []);
end;

end.

