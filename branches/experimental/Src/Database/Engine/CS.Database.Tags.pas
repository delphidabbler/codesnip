unit CS.Database.Tags;

interface

uses
  SysUtils,
  Generics.Collections,
  CS.Database.Types;

type
  TDBTagList = class(TInterfacedObject, IDBTagList)
  strict private
    var
      fTags: TList<TDBTag>;
  public
    constructor Create; overload;
    constructor Create(ATags: IDBTagList); overload;
    destructor Destroy; override;
    { IDBTagList methods }
    function GetEnumerator: TEnumerator<TDBTag>; inline;
    procedure Add(const ATag: TDBTag);
    procedure Delete(const ATag: TDBTag);
    procedure Clear; inline;
    function Contains(const ATag: TDBTag): Boolean; inline;
    function GetItem(const Idx: Integer): TDBTag; inline;
    function GetCount: Integer; inline;
    property Items[const Idx: Integer]: TDBTag read GetItem;
    property Count: Integer read GetCount;
  end;

  EDBTagList = class(Exception);

implementation

uses
  Classes,
  RTLConsts;

{ TDBTagList }

procedure TDBTagList.Add(const ATag: TDBTag);
begin
//  if Contains(ATag) then
//    raise EListError.Create(SGenericDuplicateItem);
  fTags.Add(ATag);
end;

procedure TDBTagList.Clear;
begin
  fTags.Clear;
end;

function TDBTagList.Contains(const ATag: TDBTag): Boolean;
begin
  Result := fTags.IndexOf(ATag) >= 0;
end;

constructor TDBTagList.Create(ATags: IDBTagList);
var
  Tag: TDBTag;
begin
  Create;
  for Tag in ATags do
    fTags.Add(Tag);
end;

constructor TDBTagList.Create;
begin
  inherited Create;
  fTags := TList<TDBTag>.Create(TDBTag.TComparer.Create);
end;

procedure TDBTagList.Delete(const ATag: TDBTag);
var
  Idx: Integer;
begin
  Idx := fTags.IndexOf(ATag);
  if Idx = -1 then
    raise EListError.Create(SGenericItemNotFound);
  fTags.Delete(Idx);
end;

destructor TDBTagList.Destroy;
begin
  fTags.Free;
  inherited;
end;

function TDBTagList.GetCount: Integer;
begin
  Result := fTags.Count;
end;

function TDBTagList.GetEnumerator: TEnumerator<TDBTag>;
begin
  Result := fTags.GetEnumerator;
end;

function TDBTagList.GetItem(const Idx: Integer): TDBTag;
begin
  Result := fTags[Idx];
end;

end.
