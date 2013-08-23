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
 * Basic types used in the snippets database engine.
}


unit CS.Database.Types;

interface

uses
  SysUtils,
  Generics.Collections, Generics.Defaults,
  CS.Markup,
  CS.Utils.Dates;

type
  EDBSnippetID = class(Exception);

  TDBSnippetID = record
  public
    type
      TEqualityComparer = class(TEqualityComparer<TDBSnippetID>)
      public
        function Equals(const Left, Right: TDBSnippetID): Boolean; override;
        function GetHashCode(const Value: TDBSnippetID): Integer; override;
      end;
  strict private
    var
      fID: string;
  public
    constructor Create(const AIDStr: string);
    class function CreateNew: TDBSnippetID; static;
    class operator Equal(const Left, Right: TDBSnippetID): Boolean; inline;
    class operator NotEqual(const Left, Right: TDBSnippetID): Boolean; inline;
    function ToString: string; inline;
    function Hash: Integer; inline;
    class function IsValidIDString(const S: string): Boolean; static;
  end;

  TDBLanguage = record
  strict private
    const
      DefaultLanguage = 'Text';
    var
      fName: string;
  public
    constructor Create(const AName: string);
    class operator Equal(const Left, Right: TDBLanguage): Boolean; inline;
    class operator NotEqual(const Left, Right: TDBLanguage): Boolean; inline;
    class function Compare(const Left, Right: TDBLanguage): Integer; static;
      inline;
    class function CreateDefault: TDBLanguage; static; inline;
    function CompareTo(const Other: TDBLanguage): Integer; inline;
    function ToString: string; inline;
  end;

  TDBSnippetProp = (
    spID, spTitle, spDescription, spSourceCode, spLanguage, spModified
  );

  TDBSnippetProps = set of TDBSnippetProp;

  ISnippetBase = interface(IInterface)
    ['{0F915A15-441B-4180-A9C2-41C52AF63C8A}']
    function GetID: TDBSnippetID;
    function GetTitle: string;
    function GetDescription: TMarkup;
    function GetSourceCode: string;
    function GetLanguage: TDBLanguage;
    function GetModified: TUTCDateTime;

    property ID: TDBSnippetID read GetID;
    property Modified: TUTCDateTime read GetModified;
  end;

  IReadOnlySnippet = interface(ISnippetBase)
    ['{BD221CF4-482D-4FF9-BDAE-D320DDEBD578}']
    function GetValidProperties: TDBSnippetProps;

    property Title: string read GetTitle;
    property Description: TMarkup read GetDescription;
    property SourceCode: string read GetSourceCode;
    property Language: TDBLanguage read GetLanguage;
    property ValidProperties: TDBSnippetProps read GetValidProperties;

    function SupportsProperty(const AProp: TDBSnippetProp): Boolean;
  end;

  ISnippet = interface(ISnippetBase)
    ['{75F44D0E-7611-4A8D-A4F5-F7612E466238}']
    procedure SetTitle(const ATitle: string);
    procedure SetDescription(const ADescription: TMarkup);
    procedure SetSourceCode(const ASourceCode: string);
    procedure SetLanguage(const ALanguage: TDBLanguage);

    property Title: string read GetTitle write SetTitle;
    property Description: TMarkup read GetDescription write SetDescription;
    property SourceCode: string read GetSourceCode write SetSourceCode;
    property Language: TDBLanguage read GetLanguage write SetLanguage;
  end;

  IDBSnippetIDList = interface(IInterface)
    ['{842B9A92-6CD5-4DC3-9DA7-9753B08A52AB}']
    function GetEnumerator: TEnumerator<TDBSnippetID>;
    procedure Add(const ID: TDBSnippetID);
    procedure Delete(const ID: TDBSnippetID);
    procedure Clear;
    function Contains(const ID: TDBSnippetID): Boolean;
    function GetItem(const Idx: Integer): TDBSnippetID;
    function GetCount: Integer;
    property Items[const Idx: Integer]: TDBSnippetID read GetItem;
    property Count: Integer read GetCount;
  end;

  TDBFilter = reference to function (ASnippet: IReadOnlySnippet): Boolean;

implementation

uses
  Character,

  CS.Utils.Hashes,
  UStrUtils;

{ TDBSnippetID }

constructor TDBSnippetID.Create(const AIDStr: string);
resourcestring
  sInvalidIDStr = '"%s" is not a valid snippet ID string';
begin
  if not IsValidIDString(AIDStr) then
    raise EDBSnippetID.CreateFmt(sInvalidIDStr, [AIDStr]);
  fID := AIDStr;
end;

class function TDBSnippetID.CreateNew: TDBSnippetID;
var
  Bytes: TBytes;
  B: Byte;
  IDStr: string;
begin
  Bytes := TGUID.NewGuid.ToByteArray;
  IDStr := EmptyStr;
  for B in Bytes do
    IDStr := IDStr + IntToHex(B, 2 * SizeOf(B));
  Result := TDBSnippetID.Create(IDStr);
end;

class operator TDBSnippetID.Equal(const Left, Right: TDBSnippetID): Boolean;
begin
  Result := Left.fID = Right.fID;
end;

function TDBSnippetID.Hash: Integer;
begin
  Result := Integer(PaulLarsonHash(fID));
end;

class function TDBSnippetID.IsValidIDString(const S: string): Boolean;

  function IsValidChar(const Ch: Char): Boolean;
  const
    ValidPunctChars = ['_', '-'];
  begin
    Result := TCharacter.IsLetterOrDigit(Ch) or CharInSet(Ch, ValidPunctChars);
  end;

var
  Ch: Char;
begin
  // check for empty string
  if S = EmptyStr then
    Exit(False);
  // check for valid characters
  for Ch in S do
    if not IsValidChar(Ch) then
      Exit(False);
  Result := True;
end;

class operator TDBSnippetID.NotEqual(const Left, Right: TDBSnippetID): Boolean;
begin
  Result := Left.fID <> Right.fID;
end;

function TDBSnippetID.ToString: string;
begin
  Result := fID;
end;

{ TDBSnippetID.TEqualityComparer }

function TDBSnippetID.TEqualityComparer.Equals(const Left,
  Right: TDBSnippetID): Boolean;
begin
  Result := Left = Right;
end;

function TDBSnippetID.TEqualityComparer.GetHashCode(
  const Value: TDBSnippetID): Integer;
begin
  Result := Value.Hash;
end;

{ TDBLanguage }

class function TDBLanguage.Compare(const Left, Right: TDBLanguage): Integer;
begin
  Result := StrCompareText(Left.fName, Right.fName);
end;

function TDBLanguage.CompareTo(const Other: TDBLanguage): Integer;
begin
  Result := Compare(Self, Other);
end;

constructor TDBLanguage.Create(const AName: string);
begin
  fName := AName;
end;

class function TDBLanguage.CreateDefault: TDBLanguage;
begin
  Result := TDBLanguage.Create(DefaultLanguage);
end;

class operator TDBLanguage.Equal(const Left, Right: TDBLanguage): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

class operator TDBLanguage.NotEqual(const Left, Right: TDBLanguage): Boolean;
begin
  Result := Compare(Left, Right) <> 0;
end;

function TDBLanguage.ToString: string;
begin
  Result := fName;
end;

end.

