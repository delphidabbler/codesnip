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
 * Provides a class that encapsulates a set of snippet tags.
}


unit CS.Database.Tags;

interface

uses
  Collections.Base,
  Collections.Sets,
  CS.Database.Types,
  IntfCommon;

type
  TTagSet = class(TInterfacedObject, ITagSet, IClonable)
  strict private
    var
      fTags: TArraySet<TTag>;
  public
    constructor Create; overload;
    constructor Create(Tags: ITagSet); overload;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<TTag>;
    function Contains(const ATag: TTag): Boolean;
    function ContainsSubSet(ASubSet: ITagSet): Boolean;
    function GetCount: Integer;
    function IsEmpty: Boolean;
    function Filter(const AFilterFn: TTagFilter): ITagSet;
    procedure Assign(Other: ITagSet);
    procedure Add(const ATag: TTag);
    procedure Include(Tags: ITagSet);
    procedure Remove(const ATag: TTag);
    procedure Exclude(Tags: ITagSet);
    procedure Clear;
    property Count: Integer read GetCount;
    function Clone: IInterface;
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

function TTagSet.Clone: IInterface;
begin
  Result := TTagSet.Create(Self);
end;

function TTagSet.Contains(const ATag: TTag): Boolean;
begin
  Result := fTags.Contains(ATag);
end;

function TTagSet.ContainsSubSet(ASubSet: ITagSet): Boolean;
var
  Tag: TTag;
begin
  for Tag in ASubSet do
    if not fTags.Contains(Tag) then
      Exit(False);
  Result := True;
end;

constructor TTagSet.Create;
begin
  inherited Create;
  fTags := TArraySet<TTag>.Create(
    TRules<TTag>.Create(TTag.TComparator.Create, TTag.TComparator.Create)
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

procedure TTagSet.Exclude(Tags: ITagSet);
var
  Tag: TTag;
begin
  for Tag in Tags do
    fTags.Remove(Tag);
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

procedure TTagSet.Include(Tags: ITagSet);
var
  Tag: TTag;
begin
  for Tag in Tags do
    fTags.Add(Tag); // set implementation ignores duplicates
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

