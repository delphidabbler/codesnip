unit CS.Database.Snippets;

interface

uses
  Generics.Collections,
  CS.Database.Types,
  CS.Markup,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  UExceptions,
  UIStringList;

type
  TSnippetBase = class(TInterfacedObject)
  strict private
    var
      fID: TDBSnippetID;
      fCreated: TUTCDateTime;
      fModified: TUTCDateTime;
      fTitle: string;
      fDescription: TMarkup;
      fSourceCode: string;
      fLanguageID: TSourceCodeLanguageID;
      fRequiredModules: IStringList;
      fRequiredSnippets: IDBSnippetIDList;
      fXRefs: IDBSnippetIDList;
      fNotes: TMarkup;
      fKind: TDBSnippetKind;
      fCompileResults: TDBCompileResults;
      fTags: IDBTagList;
      fLinkInfo: ISnippetLinkInfo;
  strict protected
    function SupportsProperty(const APropID: TDBSnippetProp): Boolean; virtual;
  public
    constructor Create; overload;
    constructor Create(const ASnippetID: TDBSnippetID); overload;
    constructor Create(const ASourceSnippet: TSnippetBase); overload;
    destructor Destroy; override;
    procedure UpdateFrom(const ASourceSnippet: TSnippetBase);
    function GetID: TDBSnippetID; virtual;
    function GetCreated: TUTCDateTime;
    procedure SetCreated(const Value: TUTCDateTime);
    function GetModified: TUTCDateTime; virtual;
    procedure SetModified(const Value: TUTCDateTime);
    function GetTitle: string; virtual;
    procedure SetTitle(const Value: string);
    function GetDescription: TMarkup; virtual;
    procedure SetDescription(const Value: TMarkup);
    function GetSourceCode: string; virtual;
    procedure SetSourceCode(const Value: string);
    function GetLanguageID: TSourceCodeLanguageID; virtual;
    procedure SetLanguageID(const Value: TSourceCodeLanguageID);
    function GetRequiredModules: IStringList;
    procedure SetRequiredModules(Value: IStringList);
    function GetRequiredSnippets: IDBSnippetIDList;
    procedure SetRequiredSnippets(Value: IDBSnippetIDList);
    function GetXRefs: IDBSnippetIDList;
    procedure SetXRefs(Value: IDBSnippetIDList);
    function GetNotes: TMarkup;
    procedure SetNotes(const Value: TMarkup);
    function GetKind: TDBSnippetKind;
    procedure SetKind(const Value: TDBSnippetKind);
    function GetCompileResults: TDBCompileResults;
    procedure SetCompileResults(const Value: TDBCompileResults);
    function GetTags: IDBTagList;
    procedure SetTags(Value: IDBTagList);
    function GetLinkInfo: ISnippetLinkInfo;
    procedure SetLinkInfo(ALinkInfo: ISnippetLinkInfo);
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
    constructor Create(const ASnippetID: TDBSnippetID); overload;
    constructor Create(const ASourceSnippet: TSnippetBase;
      const ValidProps: TDBSnippetProps = []); overload;
    destructor Destroy; override;
    function GetModified: TUTCDateTime; override;
    function GetTitle: string; override;
    function GetDescription: TMarkup; override;
    function GetSourceCode: string; override;
    function GetLanguageID: TSourceCodeLanguageID; override;
    function GetValidProperties: TDBSnippetProps;
    function SupportsProperty(const AProp: TDBSnippetProp): Boolean; override;
  end;

  TDBSnippetIDList = class(TInterfacedObject, IDBSnippetIDList)
  strict private
    var
      fItems: TList<TDBSnippetID>;
    function IndexOf(const AID: TDBSnippetID): Integer;
  public
    constructor Create; overload;
    constructor Create(const ASnippetIDs: IDBSnippetIDList); overload;
    destructor Destroy; override;

    // IDBSnippetIDList methods
    function GetEnumerator: TEnumerator<TDBSnippetID>;
    procedure Add(const AID: TDBSnippetID);
    procedure Delete(const AID: TDBSnippetID);
    procedure Clear;
    function Contains(const AID: TDBSnippetID): Boolean; inline;
    function GetItem(const AIdx: Integer): TDBSnippetID;
    function GetCount: Integer;
    property Items[const AIdx: Integer]: TDBSnippetID read GetItem;
    property Count: Integer read GetCount;
  end;

  EDBSnippet = class(EBug);

implementation

uses
  SysUtils,
  Classes,
  RTLConsts,
  CS.Database.Tags;

{ TDBSnippetIDList }

procedure TDBSnippetIDList.Add(const AID: TDBSnippetID);
begin
//  if Contains(AID) then
//    raise EListError.Create(SGenericDuplicateItem);
  fItems.Add(AID);
end;

procedure TDBSnippetIDList.Clear;
begin
  fItems.Clear;
end;

function TDBSnippetIDList.Contains(const AID: TDBSnippetID): Boolean;
begin
  Result := IndexOf(AID) >= 0;
end;

constructor TDBSnippetIDList.Create(const ASnippetIDs: IDBSnippetIDList);
var
  ID: TDBSnippetID;
begin
  Create;
  for ID in ASnippetIDs do
    fItems.Add(ID);
end;

constructor TDBSnippetIDList.Create;
begin
  inherited Create;
  fItems := TList<TDBSnippetID>.Create;
end;

procedure TDBSnippetIDList.Delete(const AID: TDBSnippetID);
var
  Idx: Integer;
begin
  Idx := IndexOf(AID);
  if Idx = -1 then
    raise EListError.Create(SGenericItemNotFound);
  fItems.Delete(Idx);
end;

destructor TDBSnippetIDList.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TDBSnippetIDList.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TDBSnippetIDList.GetEnumerator: TEnumerator<TDBSnippetID>;
begin
  Result := fItems.GetEnumerator;
end;

function TDBSnippetIDList.GetItem(const AIdx: Integer): TDBSnippetID;
begin
  Result := fItems[AIdx];
end;

function TDBSnippetIDList.IndexOf(const AID: TDBSnippetID): Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(fItems.Count) do
    if fItems[Idx] = AID then
      Exit(Idx);
  Result := -1;
end;

{ TSnippetBase }

constructor TSnippetBase.Create(const ASourceSnippet: TSnippetBase);
begin
  Create(ASourceSnippet.fID);
  UpdateFrom(ASourceSnippet);
end;

constructor TSnippetBase.Create(const ASnippetID: TDBSnippetID);
begin
  inherited Create;
  fID := ASnippetID;
  fCreated := TUTCDateTime.Now;
  fModified := fCreated;
  fTitle := '';
  fDescription := TMarkup.CreateEmpty;
  fSourceCode := '';
  fLanguageID := TSourceCodeLanguageID.CreateDefault;
  fRequiredModules := nil;
  fRequiredSnippets := nil;
  fXRefs := nil;
  fNotes := TMarkup.CreateEmpty;
  fKind := skFreeForm;
  fCompileResults := TDBCompileResults.CreateNull;
  fTags := nil;
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

function TSnippetBase.GetCompileResults: TDBCompileResults;
begin
  Result := fCompileResults;
end;

function TSnippetBase.GetCreated: TUTCDateTime;
begin
  Result := fCreated;
end;

function TSnippetBase.GetDescription: TMarkup;
begin
  Result := fDescription;
end;

function TSnippetBase.GetID: TDBSnippetID;
begin
  Result := fID;
end;

function TSnippetBase.GetKind: TDBSnippetKind;
begin
  Result := fKind;
end;

function TSnippetBase.GetLanguageID: TSourceCodeLanguageID;
begin
  Result := fLanguageID;
end;

function TSnippetBase.GetLinkInfo: ISnippetLinkInfo;
begin
  // TODO: Create a null link info object if fLinkInfo is nil
  // TODO: return a clone of fLinkInfo
  Result := fLinkInfo;
end;

function TSnippetBase.GetModified: TUTCDateTime;
begin
  Result := fModified;
end;

function TSnippetBase.GetNotes: TMarkup;
begin
  Result := fNotes;
end;

function TSnippetBase.GetRequiredModules: IStringList;
begin
  if not Assigned(fRequiredModules) then
    Exit(TIStringList.Create);
  Result := TIStringList.Create(fRequiredModules);
end;

function TSnippetBase.GetRequiredSnippets: IDBSnippetIDList;
begin
  if not Assigned(fRequiredSnippets) then
    Exit(TDBSnippetIDList.Create);
  Result := TDBSnippetIDList.Create(fRequiredSnippets);
end;

function TSnippetBase.GetSourceCode: string;
begin
  Result := fSourceCode;
end;

function TSnippetBase.GetTags: IDBTagList;
begin
  if not Assigned(fTags) then
    Exit(TDBTagList.Create);
  Result := TDBTagList.Create(fTags);
end;

function TSnippetBase.GetTitle: string;
begin
  Result := fTitle;
end;

function TSnippetBase.GetXRefs: IDBSnippetIDList;
begin
  if not Assigned(fXRefs) then
    Exit(TDBSnippetIDList.Create);
  Result := TDBSnippetIDList.Create(fXRefs);
end;

procedure TSnippetBase.SetCompileResults(const Value: TDBCompileResults);
begin
  fCompileResults := Value;
end;

procedure TSnippetBase.SetCreated(const Value: TUTCDateTime);
begin
  fCreated := Value;
end;

procedure TSnippetBase.SetDescription(const Value: TMarkup);
begin
  fDescription := Value;
end;

procedure TSnippetBase.SetKind(const Value: TDBSnippetKind);
begin
  fKind := Value;
end;

procedure TSnippetBase.SetLanguageID(const Value: TSourceCodeLanguageID);
begin
  fLanguageID := Value;
end;

procedure TSnippetBase.SetLinkInfo(ALinkInfo: ISnippetLinkInfo);
begin
  fLinkInfo := ALinkInfo;
end;

procedure TSnippetBase.SetModified(const Value: TUTCDateTime);
begin
  fModified := Value;
end;

procedure TSnippetBase.SetNotes(const Value: TMarkup);
begin
  fNotes := Value;
end;

procedure TSnippetBase.SetRequiredModules(Value: IStringList);
begin
  if not Assigned(Value) or (Value.Count = 0) then
    fRequiredModules := nil
  else
    fRequiredModules := TIStringList.Create(Value);
end;

procedure TSnippetBase.SetRequiredSnippets(Value: IDBSnippetIDList);
begin
  if not Assigned(Value) or (Value.Count = 0) then
    fRequiredSnippets := nil
  else
    fRequiredSnippets := TDBSnippetIDList.Create(Value);
end;

procedure TSnippetBase.SetSourceCode(const Value: string);
begin
  fSourceCode := Value;
end;

procedure TSnippetBase.SetTags(Value: IDBTagList);
begin
  if not Assigned(Value) or (Value.Count = 0) then
    fTags := nil
  else
    fTags := TDBTagList.Create(Value);
end;

procedure TSnippetBase.SetTitle(const Value: string);
begin
  fTitle := Value;
end;

procedure TSnippetBase.SetXRefs(Value: IDBSnippetIDList);
begin
  if not Assigned(Value) or (Value.Count = 0) then
    fXRefs := nil
  else
    fXRefs := TDBSnippetIDList.Create(Value);
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
end;

{ TSnippet }

class function TSnippet.CreateNew: TSnippet;
begin
  Result := TSnippet.Create(TDBSnippetID.CreateNew);
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

constructor TPartialSnippet.Create(const ASnippetID: TDBSnippetID);
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

function TPartialSnippet.GetDescription: TMarkup;
begin
  CheckValidProp(spDescription);
  Result := inherited GetDescription;
end;

function TPartialSnippet.GetLanguageID: TSourceCodeLanguageID;
begin
  CheckValidProp(spLanguageID);
  Result := inherited GetLanguageID;
end;

function TPartialSnippet.GetModified: TUTCDateTime;
begin
  CheckValidProp(spModified);
  Result := inherited GetModified;
end;

function TPartialSnippet.GetSourceCode: string;
begin
  CheckValidProp(spSourceCode);
  Result := inherited GetSourceCode;
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

function TPartialSnippet.SupportsProperty(const AProp: TDBSnippetProp): Boolean;
begin
  Result := (AProp in fValidProperties) or (fValidProperties = []);
end;

end.

