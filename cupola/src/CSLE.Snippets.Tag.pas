{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data types that encapsulate snippet tags.

  NOTE:
    This unit is closely based on code taken from the CodeSnip Pavilion
    branch's CS.Database.Tags and CS.Database.Types units.
    See https://tinyurl.com/4hnfp2jm and https://tinyurl.com/pju27zby
}

unit CSLE.Snippets.Tag;

interface

uses
  System.SysUtils,
  System.Hash,
  System.Generics.Defaults,
  System.Generics.Collections,
  Grijjy.Collections;

type 
  TTag = record
    type
      ///  <summary>Comparator for tags.</summary>
      TComparator = class(TInterfacedObject,
        IComparer<TTag>, IEqualityComparer<TTag>)
        ///  <summary>Compares tags Left and Right, returning -ve if Left less
        ///  than Right, 0 if equal or +ve if Left greater than Right.</summary>
        ///  <remarks>Method of IComparator and IComparer.</remarks>
        function Compare(const Left, Right: TTag): Integer; inline;
        ///  <summary>Checks if two tags, Left and Right, are equal.</summary>
        ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
        function Equals(const Left, Right: TTag): Boolean;
          reintroduce; overload; inline;
        ///  <summary>Gets hash of tag.</summary>
        ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
        function GetHashCode(const Value: TTag): Integer;
          reintroduce; overload; inline;
      end;
  strict private
    var
      fTag: string;
    class function IsValidTagChar(const Ch: Char): Boolean; static; inline;
    class function Compare(const Left, Right: TTag): Integer; static; inline;
  public
    const
      ///  <summary>Maximum size, in characters, of string representation of
      ///  tag.</summary>
      MaxTagStringLength = 32;
    constructor Create(const ATagStr: string);
    class function CreateNull: TTag; static;
    class operator Equal(const Left, Right: TTag): Boolean; inline;
    class operator NotEqual(const Left, Right: TTag): Boolean; inline;
    ///  <summary>Checks if a string is valid for use as a tag.</summary>
    ///  <remarks>A valid tag string contains 1..32 characters. Each character
    ///  must be either a letter, a digit, one of the characters <c>-</c>,
    ///  <c>_</c>, <c>:</c>, <c>(</c>, <c>)</c>, <c>&</c> or a space character.
    ///  Any of those characters may start a tag expect for the space
    ///  character.</remarks>
    ///  <remarks>A valid tag string is between 1 and 32 characters long must
    ///  comprise letters, digits, symbols, punctuation characters or the space
    ///  character, with the exception that a tag string may not begin with a
    ///  space character.</remarks>
    class function IsValidTagString(const AStr: string): Boolean; static;
    class function MakeValidTagString(const AStr: string): string; static;
    function IsNull: Boolean;
    function ToString: string; inline;
  end;

  TTagFilter = reference to function(const ATag: TTag): Boolean;

  ITagSet = interface(IInterface)
    ['{493A9607-0A86-4C5B-B856-36CAB264B920}']
    function GetEnumerator: TEnumerator<TTag>;
    function Contains(const ATag: TTag): Boolean; overload;
    function Contains(ASubSet: ITagSet): Boolean; overload;
    function GetCount: NativeUInt;
    function IsEmpty: Boolean;
    function Filter(const AFilterFn: TTagFilter): ITagSet;
    procedure Assign(Other: ITagSet);
    procedure Include(const ATag: TTag); overload;
    procedure Include(Tags: ITagSet); overload;
    procedure Exclude(const ATag: TTag); overload;
    procedure Exclude(Tags: ITagSet); overload;
    procedure Clear;
    property Count: NativeUInt read GetCount;
  end;

  TTagSet = class(TInterfacedObject, ITagSet)
  strict private
    var
      fTags: TgoSet<TTag>;
  public
    constructor Create; overload;
    constructor Create(Tags: ITagSet); overload;
    ///  <summary>Create a tag set from the given array of tags.</summary>
    ///  <exception><c>EListError</c> raised if <c>ATags</c> contains any
    ///  duplicate tags.</exception>
    constructor Create(ATags: array of TTag); overload;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TTag>; inline;
    function Contains(const ATag: TTag): Boolean; overload;
    function Contains(ASubSet: ITagSet): Boolean; overload;
    function GetCount: NativeUInt; inline;
    function IsEmpty: Boolean; inline;
    function Filter(const AFilterFn: TTagFilter): ITagSet;
    procedure Assign(Other: ITagSet); overload;
    procedure Assign(ATags: array of TTag); overload;
    procedure Include(const ATag: TTag); overload; inline;
    procedure Include(Tags: ITagSet); overload;
    procedure Exclude(const ATag: TTag); overload; inline;
    procedure Exclude(Tags: ITagSet); overload;
    procedure Clear; inline;
  end;

implementation

uses
  System.Types,
  System.Character,
  CSLE.Exceptions;

{ TTag }

class function TTag.Compare(const Left, Right: TTag): Integer;
begin
  Result := string.CompareText(Left.fTag, Right.fTag);
end;

constructor TTag.Create(const ATagStr: string);
resourcestring
  sBadTagStr = '"%s" is not a valid tag string';
begin
  if not IsValidTagString(ATagStr) then
    raise EUnexpected.CreateFmt(sBadTagStr, [ATagStr]);
  fTag := ATagStr;
end;

class function TTag.CreateNull: TTag;
begin
  Result.fTag := string.Empty;
end;

class operator TTag.Equal(const Left, Right: TTag): Boolean;
begin
  Result := Compare(Left, Right) = EqualsValue;
end;

function TTag.IsNull: Boolean;
begin
  Result := fTag.IsEmpty;
end;

class function TTag.IsValidTagChar(const Ch: Char): Boolean;
begin
  Result := Ch.IsLetterOrDigit
    or Ch.IsPunctuation
    or Ch.IsSymbol
    or (Ch = ' ');
end;

class function TTag.IsValidTagString(const AStr: string): Boolean;
begin
  Result := False;
  if AStr.IsEmpty then
    Exit;
  if Length(AStr) > MaxTagStringLength then
    Exit;
  if AStr[1] = ' ' then
    Exit;
  for var Ch in AStr do
    if not IsValidTagChar(Ch) then
      Exit;
  Result := True;
end;

class function TTag.MakeValidTagString(const AStr: string): string;
const
  InvalidCharSubstitue = '_'; // char used to replace invalid chars in tags
begin
  if AStr.IsEmpty then
    raise EUnexpected.Create('TTag.MakeValidTagString: AStr can''t be empty');
  SetLength(Result, Length(AStr));
  for var I := 1 to Length(AStr) do
  begin
    if IsValidTagChar(AStr[I]) then
      Result[I] := AStr[I]
    else
      Result[I] := InvalidCharSubstitue;
  end;
  if Result[1] = ' ' then
    Result[1] := InvalidCharSubstitue;
end;

class operator TTag.NotEqual(const Left, Right: TTag): Boolean;
begin
  Result := Compare(Left, Right) <> EqualsValue;
end;

function TTag.ToString: string;
begin
  Result := fTag;
end;

{ TTag.TComparator }

function TTag.TComparator.Compare(const Left, Right: TTag): Integer;
begin
  Result := TTag.Compare(Left, Right);
end;

function TTag.TComparator.Equals(const Left, Right: TTag): Boolean;
begin
  Result := Left = Right;
end;

function TTag.TComparator.GetHashCode(const Value: TTag): Integer;
begin
  Result := THashBobJenkins.GetHashValue(Value.ToString);
end;

{ TTagSet }

procedure TTagSet.Assign(Other: ITagSet);
begin
  Assert(Assigned(Other), ClassName + '.Assign: Other must not be nil');
  Clear;
  for var Tag in Other do
    fTags.Add(Tag);
end;

procedure TTagSet.Assign(ATags: array of TTag);
begin
  Clear;
  for var Tag in ATags do
    fTags.Add(Tag);
end;

procedure TTagSet.Clear;
begin
  fTags.Clear;
end;

function TTagSet.Contains(const ATag: TTag): Boolean;
begin
  Result := fTags.Contains(ATag);
end;

function TTagSet.Contains(ASubSet: ITagSet): Boolean;
begin
  for var Tag in ASubSet do
    if not fTags.Contains(Tag) then
      Exit(False);
  Result := True;
end;

constructor TTagSet.Create(ATags: array of TTag);
begin
  Create;
  Assign(ATags);
end;

constructor TTagSet.Create(Tags: ITagSet);
begin
  Assert(Assigned(Tags), ClassName + '.Create: Tags must not be nil');
  Create;
  Assign(Tags);
end;

constructor TTagSet.Create;
begin
  inherited;
  fTags := TgoSet<TTag>.Create(TTag.TComparator.Create);
end;

destructor TTagSet.Destroy;
begin
  fTags.Free;
  inherited;
end;

procedure TTagSet.Exclude(const ATag: TTag);
begin
  fTags.Remove(ATag);
end;

procedure TTagSet.Exclude(Tags: ITagSet);
begin
  Assert(Assigned(Tags), ClassName + '.Exclude: Tags must not be nil');
  for var Tag in Tags do
    fTags.Remove(Tag);
end;

function TTagSet.Filter(const AFilterFn: TTagFilter): ITagSet;
begin
  Assert(Assigned(AFilterFn), ClassName + '.Filter: AFilterFn not assigned');
  Result := TTagSet.Create;
  for var Tag in fTags do
    if AFilterFn(Tag) then
      Result.Include(Tag);
end;

function TTagSet.GetCount: NativeUInt;
begin
  Result := NativeUInt(fTags.Count);
end;

function TTagSet.GetEnumerator: TEnumerator<TTag>;
begin
  Result := fTags.GetEnumerator;
end;

procedure TTagSet.Include(Tags: ITagSet);
begin
  Assert(Assigned(Tags), ClassName + '.Include: Tags must not be nil');
  for var Tag in Tags do
    fTags.AddOrSet(Tag);
end;

procedure TTagSet.Include(const ATag: TTag);
begin
  fTags.AddOrSet(ATag);
end;

function TTagSet.IsEmpty: Boolean;
begin
  Result := fTags.Count = 0;
end;

end.
