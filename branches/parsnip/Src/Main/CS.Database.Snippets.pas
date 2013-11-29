unit CS.Database.Snippets;

interface

uses
  Generics.Collections,
  CS.ActiveText,
  Compilers.UGlobals,
  CS.Database.Types,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
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

  TSnippet = class(TSnippetBase, ISnippet)
  public
    class function CreateNew: TSnippet;
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

//  TDBSnippetIDList = class(TInterfacedObject, IDBSnippetIDList)
//  strict private
//    var
//      fItems: TList<TDBSnippetID>;
//    function IndexOf(const AID: TDBSnippetID): Integer;
//  public
//    constructor Create; overload;
//    constructor Create(const ASnippetIDs: IDBSnippetIDList); overload;
//    destructor Destroy; override;
//
//    // IDBSnippetIDList methods
//    function GetEnumerator: TEnumerator<TDBSnippetID>;
//    procedure Add(const AID: TDBSnippetID);
//    procedure Delete(const AID: TDBSnippetID);
//    procedure Clear;
//    function Contains(const AID: TDBSnippetID): Boolean; inline;
//    function GetItem(const AIdx: Integer): TDBSnippetID;
//    function GetCount: Integer;
//    property Items[const AIdx: Integer]: TDBSnippetID read GetItem;
//    property Count: Integer read GetCount;
//  end;

  EDBSnippet = class(EBug);

implementation

uses
  SysUtils,
  Classes,
  RTLConsts,
  CS.Database.SnippetLinks,
  CS.Database.Tags,
  IntfCommon,
  USnippetIDs;

//{ TDBSnippetIDList }
//
//procedure TDBSnippetIDList.Add(const AID: TDBSnippetID);
//begin
////  if Contains(AID) then
////    raise EListError.Create(SGenericDuplicateItem);
//  fItems.Add(AID);
//end;
//
//procedure TDBSnippetIDList.Clear;
//begin
//  fItems.Clear;
//end;
//
//function TDBSnippetIDList.Contains(const AID: TDBSnippetID): Boolean;
//begin
//  Result := IndexOf(AID) >= 0;
//end;
//
//constructor TDBSnippetIDList.Create(const ASnippetIDs: IDBSnippetIDList);
//var
//  ID: TDBSnippetID;
//begin
//  Create;
//  for ID in ASnippetIDs do
//    fItems.Add(ID);
//end;
//
//constructor TDBSnippetIDList.Create;
//begin
//  inherited Create;
//  fItems := TList<TDBSnippetID>.Create;
//end;
//
//procedure TDBSnippetIDList.Delete(const AID: TDBSnippetID);
//var
//  Idx: Integer;
//begin
//  Idx := IndexOf(AID);
//  if Idx = -1 then
//    raise EListError.Create(SGenericItemNotFound);
//  fItems.Delete(Idx);
//end;
//
//destructor TDBSnippetIDList.Destroy;
//begin
//  fItems.Free;
//  inherited;
//end;
//
//function TDBSnippetIDList.GetCount: Integer;
//begin
//  Result := fItems.Count;
//end;
//
//function TDBSnippetIDList.GetEnumerator: TEnumerator<TDBSnippetID>;
//begin
//  Result := fItems.GetEnumerator;
//end;
//
//function TDBSnippetIDList.GetItem(const AIdx: Integer): TDBSnippetID;
//begin
//  Result := fItems[AIdx];
//end;
//
//function TDBSnippetIDList.IndexOf(const AID: TDBSnippetID): Integer;
//var
//  Idx: Integer;
//begin
//  for Idx := 0 to Pred(fItems.Count) do
//    if fItems[Idx] = AID then
//      Exit(Idx);
//  Result := -1;
//end;

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

{ TSnippet }

class function TSnippet.CreateNew: TSnippet;
begin
  Result := TSnippet.Create(TSnippetID.CreateNew);
end;

destructor TSnippet.Destroy;
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

