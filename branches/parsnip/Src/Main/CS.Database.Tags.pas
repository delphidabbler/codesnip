unit CS.Database.Tags;

interface

uses
  Collections.Base,
  Collections.Sets,
  CS.Database.Types;

type
  TTagSet = class(TInterfacedObject, ITagSet)
  strict private
    var
      fTags: TArraySet<TTag>;
  public
    constructor Create; overload;
    constructor Create(Tags: ITagSet); overload;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<TTag>;
    function Contains(const ATag: TTag): Boolean;
    function GetCount: Integer;
    function IsEmpty: Boolean;
    function Filter(const AFilterFn: TTagFilter): ITagSet;
    procedure Assign(Other: ITagSet);
    procedure Add(const ATag: TTag);
    procedure Remove(const ATag: TTag);
    procedure Clear;
    property Count: Integer read GetCount;
  end;

implementation

{ TTagSet }

procedure TTagSet.Add(const ATag: TTag);
begin
  fTags.Add(ATag);
end;

procedure TTagSet.Assign(Other: ITagSet);
var
  Tag: TTag;
begin
  Clear;
  for Tag in Other do
    Add(Tag);
end;

procedure TTagSet.Clear;
begin
  fTags.Clear;
end;

function TTagSet.Contains(const ATag: TTag): Boolean;
begin
  Result := fTags.Contains(ATag);
end;

constructor TTagSet.Create;
begin
  inherited Create;
  fTags := TArraySet<TTag>.Create(
    TRules<TTag>.Create(TTag.TComparer.Create, TTag.TEqualityComparer.Create)
  );
end;

constructor TTagSet.Create(Tags: ITagSet);
begin
  Create;
  Assign(Tags);
end;

destructor TTagSet.Destroy;
begin
  fTags.Free;
  inherited;
end;

function TTagSet.Filter(const AFilterFn: TTagFilter): ITagSet;
var
  Tag: TTag;
begin
  Assert(Assigned(AFilterFn), ClassName + '.Filter: AFilterFn not assigned');
  Result := Create;
  for Tag in fTags do
    if AFilterFn(Tag) then
      Result.Add(Tag);
end;

function TTagSet.GetCount: Integer;
begin
  Result := fTags.Count;
end;

function TTagSet.GetEnumerator: IEnumerator<TTag>;
begin
  Result := fTags.GetEnumerator;
end;

function TTagSet.IsEmpty: Boolean;
begin
  Result := fTags.Empty;
end;

procedure TTagSet.Remove(const ATag: TTag);
begin
  fTags.Remove(ATag);
end;

end.
